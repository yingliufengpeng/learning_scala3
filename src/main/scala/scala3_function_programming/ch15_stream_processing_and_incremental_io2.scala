package scala3_function_programming
import scala3_function_programming.ch13_external_effects_and_io2.{IO, fahrenheitToCelsius}
import scala3_function_programming.ch11_monads.MonadCatch
import scala3_function_programming.ch10_monoids.Monoid
import scala3_function_programming.ch15_stream_processing_and_incremental_io2.Process.join

import java.io.{BufferedReader, FileReader, FileWriter}
import java.sql.{Connection, PreparedStatement}

object ch15_stream_processing_and_incremental_io2 {
  
  enum Process[F[_], O]:
    self =>
    
    case Await[FF[_], A, OO](req: FF[A], recv: Either[Throwable, A] => Process[FF, OO]) extends Process[FF, OO]
    case Emit(head: O, tail: Process[F, O])
    case Halt[FF[_], OO](err: Throwable) extends Process[FF, OO]
    
    import Process.{Process1, Sink}
    
    def map[O2](f: O => O2): Process[F, O2] = this match
      case Halt(err) => Halt(err)
      case Emit(v, t) => Emit(f(v), t map f)
      case Await(req, recv) => Await(req, recv andThen (_ map f))
    
    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match
      case Halt(err) => Halt(err)
      case Emit(v, t) => Process.Try(f(v)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
  
    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match
      case Halt(e) => Process.Try(f(e))
      case Emit(v, t) => Emit(v, t onHalt f)
      case Await(req, recv) => Await(req, recv andThen (_ onHalt f))
  
    def ++ (p: => Process[F, O]): Process[F, O] =
      this.onHalt {
        case Process.End => p    // Consult p only on normarl termination
        case err => Halt(err)   // otherwise, keep the current error
      }
    
    def repeat: Process[F, O] = this match
      case Halt(err) => Halt(err)
      case Emit(v, t) => Emit(v, t.repeat)
      case p@Await(req, recv) => Await(req, recv andThen (_.repeat))
    

    def runLog(using m_f: MonadCatch[F]): F[IndexedSeq[O]] =  
      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
        cur match
          case Emit(h, t) => go(t, acc :+ h)
          case Halt(Process.End) => m_f.unit(acc)
          case Halt(err) => m_f.fail(err)
          case Await(req, recv) => m_f.attempt(req) flatMap ( e => go(Process.Try(recv(e)), acc))
      
      go(this, IndexedSeq.empty[O])
    
    def asFinalizer: Process[F, O] = this match
      case Emit(v, t) => Emit(v, t.asFinalizer)
      case Halt(err) => Halt(err)
      case Await(req, recv) => Process.await(req) {
        case Left(Process.Kill) => this.asFinalizer
        case x => recv(x)
      }
  
    def onComplete(p: => Process[F, O]): Process[F, O] =  
      this.onHalt {
        case Process.End => p.asFinalizer
        case err => p.asFinalizer ++ Halt(err) 
      }
      
    final def drain[O2]: Process[F, O2] = this match
      case Halt(e) => Halt(e)
      case Emit(_, t) => t.drain
      case Await(req, recv) => Await(req, recv andThen (_.drain))
      
    @annotation.tailrec 
    final def kill[O2]: Process[F, O2] = this match
      case Await(req, recv) => recv(Left(Process.Kill)).drain.onHalt {
        case Process.Kill => Halt(Process.End)
        case e => Halt(e)
      }
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.kill 
      

    def |>[O2](p2: Process1[O, O2]): Process[F, O2] =
      p2 match
        case Halt(e) => this.kill onHalt ( Halt(e) ++ Halt(_))
        case Emit(h, t) => Emit(h, this |> t)
        case pp =>
          val ppp = pp.asInstanceOf[Await[Process.Is[O]#F, O, O2]]
          val Await(req, recv) = ppp
          this match
            case Halt(err) => Halt(err) |> Process.Try(recv(Left(err)))
            case Emit(h, t) => t |> Process.Try(recv(Right(h)))
            case Await(req0, recv0) => Await(req0, recv0 andThen (_ |> pp))
            
    def pipe[O2](p2: Process1[O, O2]): Process[F, O2] =
      this |> p2 
      
    def filter(f: O => Boolean): Process[F, O] =
      this |> Process.filter(f)
      
    def tee[O2, O3](p2: Process[F, O2])(t: Process.Tee[O, O2, O3]): Process[F, O3] =
      t match
        // If the halts gracefully kill off both inputs  
        case Halt(e) => this.kill onComplete( p2.kill onComplete(Halt(e)))
        // Emit any leading values and then recurse  
        case Emit(v, t) => Emit(v, (this tee p2)(t))
        // We check whether the request is for the left or right side   
        case pp@Await(side, recv) =>
          side.get match
            // It's a request from the left Process, and we get a witness that recv takes an O   
            case Left(_) => 
              val tt = pp.asInstanceOf[Await[Process.T[O, O2]#F, O, O3 ]]
              val Await(side, recv) = tt 
              this match
                // The Tee is requesting input from the left, which is halted, so halt.  
                case Halt(e) => p2.kill onComplete Halt(e)
                // There are values available, so feed them to the Tee.  
                case Emit(o, ot) => (ot tee p2)(Process.Try(recv(Right(o))))
                // No values are currently available, so wait for a value, and then continue
                // the tee operation  
                case Await(reqL, recvL) =>
                  // tt值虽说没有在这个分支用到,但是传递到下一层递归逻辑中
                  Process.await(reqL)(recvL andThen (this2 => this2.tee(p2)(tt))) 
                  
                  
              // It's a request form the right Process, and we get a witness that recv takes an
              // O2. Otherwise, this case is exactly analogous  
            case Right(_) =>
              val tt = pp.asInstanceOf[Await[Process.T[O, O2]#F, O2, O3 ]]
              val Await(side, recv) = tt
              p2 match
                case Halt(e) => this.kill onComplete Halt(e)
                case Emit(o2, ot) => (this tee ot)(Process.Try((recv(Right(o2)))))
                case Await(reqR, recvR) =>
                  // tt值虽说没有在这个分支用到,但是传递到下一层递归逻辑中
                  Process.await(reqR)(recvR andThen (p3 => (this tee p3)(tt)) ) 
    
    def combine[O2, O3](p: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] = ???
    
    def zipWith[O2](p: Process[F, O2])(f: (O, O2) => (O, O2)): Process[F, (O, O2)] =
      combine(p)((_, _))
                  
    def to[O2](sink: Sink[F, O]): Process[F, Unit] = {
      Process.join ( (this combine sink)((o, f) => f(o)) )
    }
    
    def once: Process[F, O] = this match
      case Halt(err) => Halt(err)
      case Emit(v, t) => Emit(v, Halt(Process.End))
      case Await(fa, recv) => Await(fa, recv andThen (_.once))
//  
    def through[O2](p2: Process[F, O => Process[F, O2]]): Process[F, O2] =
      join((this combine p2)((o, f) => f(o)))

  object Process:
    case class Is[I]():
      sealed trait F[X]
      val Get = new F[I] {}
    
    type Process1[I, O] = Process[Is[I]#F, O]
    
    case class T[I, I2]():
      sealed trait F[X]:
        def get: Either[I => X, I2 => X]
      val L = new F[I] {
        override def get: Either[I => I, I2 => I] = Left(identity)
      }
      val R = new F[I2] {
        override def get: Either[I => I2, I2 => I2] = Right(identity)
      }
    
    type Tee[I, I2, O] = Process[T[I, I2]#F, O]
    
    def L[I, I2] = T[I, I2]().L 
    def R[I, I2] = T[I, I2]().R 
    
    def Get[I]: Is[I]#F[I] = Is().Get 
    
    type Sink[F[_], O] = Process[F, O => Process[F, Unit]]
    
    type Channel[F[_], I, O] = Process[F, I => Process[F, O]]
    
    def query(conn: IO[Connection]): Channel[IO, Connection => PreparedStatement, Map[String, Any]] = ???
    
     
    // The infinite, constant stream
    def constant[A](a: A): Process[IO, A] = 
      eval[IO, A](IO(a)).repeat 
    
    def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#F, O](End)
    
    def awaitL[I, I2, O](recv: I => Tee[I, I2, O])(fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
      await[T[I, I2]#F, I, O](L) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }

    def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O])(fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
      await[T[I, I2]#F, I2, O](R) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }
      
    def emitT[I, I2, O](h: O, t1: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
      emit(h, t1)
      
    def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
      awaitL[I, I2, O](i => awaitR[I, I2, O](i2 => emitT(f(i, i2)))())().repeat 
      
    def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))
 
    
    // An Exception that indicates normal terminatio. This allows use to use 
    // Scala's exception mechanism for control flow.
    case object End extends Exception  
    
    // An Exception that indicates forceful termination. We'll see how this is used later.
    case object Kill extends Exception
    
    def emit[F[_], O](v: O, tail: Process[F, O] = Halt(End)): Process[F, O] =
      Emit(v, tail)
  
    def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
      try 
        p
      catch
        case e: Throwable => Halt(e) 
    
    def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
      Await(req, recv)
      
    
    def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
      @annotation.tailrec
      def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match
          case Emit(h, t) => go(t, acc :+ h)
          case Halt(End) => acc
          case Halt(err) => throw err
          case Await(req, recv) =>
            val next =
              try 
                recv(Right(req.run))
              catch
                case err: Throwable => recv(Left(err))
            go(next, acc)  
            
      go(src, IndexedSeq.empty)
    }
    
    def eval[F[_], A](a: F[A]): Process[F, A] =
      await(a) {
        case Right(v) => Emit(v, Halt(End))
        case Left(err) => Halt(err)
      }
       
    def eval_[F[_], A, B](fa: F[A]): Process[F, B] =
      await(fa)(_ => Halt(End))
      
    def halt1[I, O]: Process1[I, O] = Halt(End)
    
    def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] =
      Emit(h, t1)
      
    def await1[I, O](recv: I => Process1[I, O])(fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
      Await(Get[I], {
        (e: Either[Throwable, I]) => e match
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(i) => Try(recv(i))
      })
      
    def lift[I, O](f: I => O): Process1[I, O] =
      await1((i: I) => emit1(f(i)))()
      
    def filter[I](f: I => Boolean): Process1[I, I] =
      await1[I, I](i => if f(i) then emit1(i) else halt1)()
      
    def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] =
      p flatMap( e => e )
      
    def resource[R, O](acquire: IO[R])
                      (use: R => Process[IO, O])
                      (release: R => Process[IO, O]): Process[IO, O] = ???
    
    // 这样的语法要紧挨着才行，否则就是另起一行的语句.
    def lines(fileName: String): Process[IO, String] =
      resource(IO(io.Source.fromFile(fileName))) {
        src => 
          lazy val iter = src.getLines() // a stateful iterator 
          def step = if iter.hasNext then Some(iter.next) else None 
          // 只生成了一个可用的执行体的作用
          lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
            case None => Halt(End)
            case Some(line) => Emit(line, lines)
          }
          lines 
      } {
        src => eval_{ IO(src.close) }
      } 
      
    def fileW(file: String, append: Boolean = false): Sink[IO, String] =
      resource[FileWriter, String => Process[IO, Unit]] {
        IO { FileWriter(file, append)}
      } {
        w => constant(s => eval( IO(w.write(s)) ))
      } {
        w => eval_( IO( w.close  ) )
      }
     
      
  
    def test(): Unit = {
      val p: Process[IO, String] =
        await(IO(new BufferedReader(new FileReader("src/resources/lines.txt")))){
          case Right(b) => 
            def next: Process[IO, String] = await(IO(b.readLine)) {
              case Left(e) => await(IO(b.close))(_ => Halt(e))
              case Right(line) =>
                if line eq null then 
                  Halt(End)
                else 
                  Emit(line, next)
            }
            next
          case Left(err) => Halt(err)
        }
        
      val r = runLog(p).run
      println(f"r is $r")
      
    }
    
  
    def test2(): Unit = {
      val f = new BufferedReader(new FileReader("src/resources/lines.txt"))
      val r1 = f.readLine()
      val r2 = f.readLine()
      val r3 = f.readLine()
      println(f"r1 is $r1")
      println(f"r2 is $r2")
      println(f"r3 is $r3")
      f.close()
    }
  
    def test3(): Unit = {
       val converter: Process[IO, Unit] =
         lines("fahrenheit.txt")
           .filter(!_.startsWith("#"))
           .map(line => fahrenheitToCelsius(line.toDouble).toString)
           .to(fileW("celesius.txt"))
           .drain
           
    }
  
    def test4(): Unit = {
      val convertAll: Process[IO, Unit] = {
        (for
        out <- fileW("celsius.txt")
        file <- lines("fahrenheits.txt")
        _ <- lines(file).
          map(line => fahrenheitToCelsius(line.toDouble)).
          flatMap(celsius => out(celsius.toString))
          yield
            ()).drain

      }
    }
    
    
    def test5(): Unit = {
     val convertMultisink: Process[IO, Unit] = (
       for 
        file <- lines("fahrenheits.txt")
        _ <- lines(file).
          map(e => fahrenheitToCelsius(e.toDouble)).
          map(_.toString).
          to(fileW(s"$file.celsius"))
       yield 
         ()
     ).drain
    }  
  
  
  // A producer should free any underlying resources as soon as it knows it has 
  // no further values to produce, whether due to normal exhaustion or an exception
  
  // Any process d that consumes values from another process p must ensure that 
  // cleanup action of p are run before d halts
  @main def ch15_stream_processing_and_incremental_io2_start(): Unit = {
//    Process.test()
    Process.test4()
  }
}
