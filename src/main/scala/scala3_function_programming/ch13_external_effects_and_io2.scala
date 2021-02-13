package scala3_function_programming
import scala.io.StdIn
import concurrent.ExecutionContext.Implicits.global
import concurrent.{Await, ExecutionContext, Future}
import scala3_function_programming.ch12_applicative_and_traversable_functors.Monad
import scala3_function_programming.ch07_purely_funtiaonal_parallelism.Parallel.Par
import scala3_function_programming.ch07_purely_funtiaonal_parallelism.{Parallel => Par}
import scala3_function_programming.ch10_monoids.Monoid
import scala3_function_programming.ch13_external_effects_and_io2.Console.ConsoleIO

import scala.annotation

object ch13_external_effects_and_io2 {


  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0
    
  
  enum IO[A]:
    case Return(a: A) 
    case Suspend(resume: () => A) 
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
  
    def flatMap[B](f: A => IO[B]): IO[B] = {
//      println(f"多次?")
      FlatMap(this, f)
    }

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
      
    
    // We reassociate this to the right, effectively turnning (y flatMap g) flatMap f into y flatMap( a => g(a) flatMap f)
    // We're just taking advantages of the monad associativity law!
    @annotation.tailrec
    final def run: A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).run
        case Suspend(r) => f(r()).run
//        case FlatMap(y, g) => ((y flatMap g) flatMap f).run   
        case FlatMap(y, g) => (y flatMap ( a => g(a) flatMap f)).run  // In this case, io is an expressio like
          // FlatMap( FlatMap(y, g) , f ). We reassociate this to the right in order to be able to cal run in 
          // tail position, and the next iteration will match on y.


  object IO extends Monad[IO]:

    override def unit[A](a: => A): IO[A] = Return(a)

    def apply[A](a: => A): IO[A] = unit(a)

    extension [A, B](fa: IO[A])
      override def forever: IO[B] =
//        println(f"无限循环???")
        lazy val t: IO[B] = forever
        fa.flatMap (_ => t)
    
     

    override def apply[A, B](fab: IO[A => B])(fa: IO[A]): IO[B] =
      for
      f <- fab
      a <- fa
        yield
          f(a)

    def empty: IO[Unit] = Return(())

    def readLine: IO[String] = IO { StdIn.readLine }

    def printLine(msg: String): IO[Unit] =
      Suspend(() => Return(println(msg)))

    def converter: IO[Unit] =
      for
      _ <- printLine("Enter a temperature in degrerss Fahrenheit")
      d <- readLine.map(_.toDouble)
      _ <- printLine(fahrenheitToCelsius(d).toString)
        yield
          ()

    def echo: IO[Unit] = readLine.flatMap(printLine)

    def readInt: IO[Int] = readLine.map(_.toInt)

    def readInts: IO[(Int, Int)] = readInt ** readInt

    def replicateMM(n: Int): IO[List[String]] = replicateM(n, readLine)

    def test(): Unit = {
      //      val r = echo.run
      //      val r2 = converter.run
      //      val r3 = replicateMM(3).run
      //      println(s"r3 is $r3")

//            val r4 = readInt.forever.run  
      // go forver 这样的循环使用的类型
//      val p = printLine("Still going ....").forever
//      println(f"p is $p")
//      p.run
      
      val f: Int => IO[Int] =
        println("fff")
        (x: Int) => Return(x)
      val g = List.fill(100)(f).foldLeft(f) {
        (acc, fe) => 
            x =>
              Suspend(() => acc(x).flatMap(fe).run)
      }
//      
      val r = g(32).run 
      println(f"r is $r")
      
      val r2 = unit(3)
      println(f"r is ${r2.run}")

    }
  
  enum Async[A]:
    case Return(a: A) 
    case Suspend(resume: Par[A]) 
    case FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

    def flatMap[B](f: A => Async[B]): Async[B] = {
      //      println(f"多次?")
      FlatMap(this, f)
    }

    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  
    @annotation.tailrec 
    final def step: Async[A] = this match
      case FlatMap(FlatMap(x, f), g) => (x flatMap ( a => f(a) flatMap g)).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this 
  
    def run: Par[A] = this.step match
      case Return(a) => Par.unit(a)
      case Suspend(r) => r.flatMap(a => Par.unit(a))
      case FlatMap(x, f) => x match
        case Suspend(r) => r.flatMap(a => f(a).run)
        case _ => sys.error(s"Impossible; `step` eliminates these cases")



  enum Free[F[_], A]:
    case Return(a: A)
    case Suspend(resume: F[A])
    case FlatMap[FF[_], AA, B](sub: Free[FF, AA], k: AA => Free[FF, B]) extends Free[FF, B]
    
    type Self_Type = [X] =>> Free[F, X]

    def flatMap[B](f: A => Self_Type[B]): Self_Type[B] = 
      FlatMap(this, f)
   
     
    def map[B](f: A => B): Self_Type[B] =
      flatMap(f andThen (Return(_)))

    @annotation.tailrec
    final def step: Self_Type[A] = this match
      case FlatMap(FlatMap(x, f), g) => (x flatMap ( a => f(a) flatMap g)).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this

    def run(using f_m: Monad[F]): F[A] = this.step match
      case Return(a) => f_m.unit(a)
      case Suspend(r) => r.flatMap(a => f_m.unit(a))
      case FlatMap(x, f) => x match
        case Suspend(r) => r.flatMap(a => f(a).run)
        case _ => sys.error(s"Impossible; `step` eliminates these cases")
  
  object Free:
    implicit def freeMonad[F[_]]: Monad[[X] =>> Free[F, X]] = new Monad[[X] =>> Free[F, X]] {
      type FT = [X] =>> Free[F, X]
      override def unit[A](a: => A): FT[A] = Return(a)
    }
  
    // 尾递归优化的版本需要自己重新思考
    //https://stackoverflow.com/questions/31622000/scala-expanded-syntax-of-trampolining-function-breaks-tail-recursion
    // 这个答案以后需要思考一下
    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0, A]): A = a match
      case Return(v) => v
      case Suspend(r) => r()
      case FlatMap(x, f) => runTrampoline(x flatMap f )
   
    def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(using m_g: Monad[G]): G[A] =  
      free.step match
        case Return(a) => m_g.unit(a)
        case Suspend(r) => t(r)
        case FlatMap(Suspend(r), f) => t(r).flatMap(a => runFree(f(a))(t))
        case _ => sys.error("Impossible; `step` eliminates theses cases")
       
  
    def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] =  
      given m_g: Monad[G] = summon[Monad[G]]
      type FreeG = [X] =>> Free[G, X]
      val t = new (F ~> FreeG) {
        override def apply[A](fa: F[A]): FreeG[A] = Suspend(fg(fa))
      }
      runFree(f)(t)
       
     


  type FailRec[A] = Free[Function0, A]
  type Async2[A] = Free[Par, A]
  
  trait Translate[F[_], G[_]]: 
    def apply[A](f: F[A]): G[A]
  
  type ~> = [F[_], G[_]] =>> Translate[F, G]
  
  
  sealed trait Console[A]:
    def toPar: Par[A]
    def toThunk: () => A 
    def toReader: ConsoleReader[A] = ConsoleReader(_ =>  toThunk())
  
  case object Readline extends Console[Option[String]]:
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run  
    
    def run: Option[String] =
      try 
        Some(StdIn.readLine())
      catch
        case e: Exception => None

  case class PrintLine(line: String) extends Console[Unit]:
    def toPar: Par[Unit] = Par.lazyUnit(println(line))
    def toThunk: () => Unit = () => println(line)
  
  
  object Console:
    import Free._ 
    type ConsoleIO[A] = Free[Console, A]
    import concurrent.ExecutionContext.Implicits.global
    
    given function0Monad: Monad[Function0] = new Monad[Function0] {
      override def unit[A](a: => A): () => A = () => a 
      extension [A, B](fa: Function0[A])
        override def flatMap(f: A => Function0[B]): Function0[B] =
          () => f(fa())()
    }

    given parMonad: Monad[Par] = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)
      
//      extension [A, B](fa: Par[A])
//        override def flatMap(f: A => Par[B]): Par[B] =
//          given ExecutionContext = global
//          Par.fork(fa.flatMap(f))
    }
    
    def readLn: ConsoleIO[Option[String]] = Suspend(Readline)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
    
    val consoleToFunction0 = new (Console ~> Function0) {
      def apply[A](a: Console[A]): Function0[A] =  a.toThunk
    }
    val consoleToPar = new (Console ~> Par) {
      def apply[A](a: Console[A]): Par[A] = a.toPar
    }
    
    def runConsoleFunction0[A](a: Free[Console, A]): () => A =  
      given m: Monad[Function0] = summon[Monad[Function0]]
      runFree[Console, Function0, A](a)(consoleToFunction0)
    
    def runConsolePar[A](a: Free[Console, A]): Par[A] =
      given m: Monad[Par] = summon[Monad[Par]]
      runFree[Console, Par, A](a)(consoleToPar)
    
    def runConsole[A](fa: Free[Console, A]): A =
      runTrampoline(translate(fa)(new (Console ~> Function0) {
        def apply[A](c: Console[A]): Function0[A] = c.toThunk 
      }))


    def test(): Unit = {
      val f1 =  
        for 
          _ <- printLn("I can only interact with the console")
          ln <- readLn
        yield 
          ln 
       
    }
  
  case class ConsoleReader[A](run: String => A):
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
      
  object ConsoleReader:
    given monad: Monad[ConsoleReader] = new Monad[ConsoleReader] {
      override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)
    }
  
    val consoleToReader = new (Console ~> ConsoleReader) {
        def apply[A](a: Console[A]) = a.toReader
    }
  
//    @annotation.tailrec
    def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] = {
      given m_g: Monad[ConsoleReader] = summon[Monad[ConsoleReader]]
      Free.runFree[Console, ConsoleReader, A](io)(consoleToReader)
    }
  
  trait HandleR 
  trait HandleW 
  
  enum Files[A]:
    case OpenRead(file: String) extends Files[HandleR]
    case OpenWrite(file: String) extends Files[HandleW]
    case ReadLine(h: HandleR) extends Files[Option[String]]
    case WriteLine(h: HandleW) extends Files[Unit]

    def windowed[B](n: Int, l: List[A])(f: A => B)(using m: Monoid[B]): List[B] = ???
  
  object Files:
    val r = 3
  
//    def loop(f: HandleR, c: HandleW): Free[Files, Unit] =
//      for 
//        line <- Suspend(ReadLine(f))
//        _ <- line match
//          case None => IO.unit(())
//          case Some(s) => Suspend{ 
//            WriteLine(fahrenheitToCelsius(s.toDouble))
//          } flatMap(_ => loop(f, c))
//      yield 
//        b  

  @main def external_effectes_and_io2_start(): Unit = {
    
    IO.test()
    Console.test()
  }

}
