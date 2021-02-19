package scala3_function_programming
import scala.io.Source
import scala3_function_programming.ch11_monads.Monad
import scala3_function_programming.ch13_external_effects_and_io2.{IO, fahrenheitToCelsius}

import java.io.File

object ch15_stream_processing_and_incremental_io {
  
  // 抽象的流处理过程,不过该如何理解这些使用的思路呢?
  enum Process[I, O]: // 存储计算流,存储可用的结果!!!
    case Emit(head: O, tail: Process[I, O])         // 只有一个数据产生
    case Await(recv: Option[I] => Process[I, O])    // 数据触发的计算流
    case Halt()                                     // 停机 
    
    def apply(s: LazyList[I]): LazyList[O] = this match
      case Halt() => LazyList.empty
      case Await(recv) => s match
        case h #:: t => recv(Some(h)).apply(t)
        case xs => recv(None)(xs)  // Stream is empty
      case Emit(h, t) => h #:: t.apply(s)
    
    def repeat: Process[I, O] =
      def go(p: Process[I, O]): Process[I, O] = p match
        case Halt() => go(this) // Restart the process if it halts on its own
        case Await(recv) => Await { // recv is function 
          case None => recv(None) // Don't repeat if terminated from source 也是递归终止条件
          case p => go(recv(p))
        }
        case Emit(h, t) => Emit(h, go(t)) // 类似于map,不过是对数据类型进行转换的操作
      go(this)
    // refer
    // https://hyleung.github.io/fpinscala/fpinscala/chapter_notes/2015/11/04/chapter-15-stream-processing-and-incremental-io.html
    def |>[O2](g: Process[O, O2]): Process[I, O2] = g match
      case Halt() => Halt() // if p2 is a halt, don't bother
      case Emit(v, t) => Emit(v, this |> t) // if p2 emits, emit the value and compose this with the tail
      case pp@Await(recv) => this match
        case Halt() => Halt() |> recv(None) // if this is a halt, pass None to recv
        case Emit(v, t) => t |> recv(Some(v))
        case Await(recv2) => Await(recv2(_) |> pp )
  
    // 自我实现
    def |>>> [O2](p2: Process[O, O2]): Process[I, O2] = 
      this match
        case Halt() => Halt()
        case Emit(h, t) => 
          p2 match
            case Halt() => Halt()
            case Emit(h2, t2) => Emit(h2, t |>>> t2)
            case Await(recv) => t |>>> recv(Some(h))
        case p@Await(rec) =>
          p2 match
            case Halt() => Halt()
            case Emit(h2, t2) => Emit(h2, p |>>> t2)
            case pp@Await(_) => Await (rec(_) |>>> pp)
            
    def map[O2](f: O => O2): Process[I, O2] = 
      this |> Process.lift(f)
      
    def ++(p: => Process[I, O]): Process[I, O] = this match
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await( recv andThen (_ ++ p) )
  
    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match
      case Halt() => Halt()
      case Emit(v, t) => f(v) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    
    def orElse(p: => Process[I, O]): Process[I, O] = this match
      case Halt() => p
      case pp@Emit(_, _) => pp
      case Await(recv) => Await(recv(_).orElse(p))
   
    def zipWithIndex: Process[I, (O, Int)] =
      Process.zip(this, Process.count)
  
  object Process:
    def liftOne[I, O](f: I => O): Process[I, O] =
      Await{
        case Some(v) => Emit(f(v), tail=Halt())
        case None => Halt() 
      }
      
    def emit[I](v: I): Process[I, I] = Emit(v, Halt())
      
    def lift[I, O](f: I => O): Process[I, O] =
      liftOne(f).repeat
      
    def await[I, O](f: I => O): Process[I, O] =
      liftOne(f)
    
    def filter[I](p: I => Boolean): Process[I, I] =
      Await[I, I] {
        case Some(i) if p(i) => Emit(i, Halt())
        case _ => Halt()
      }.repeat
      
    def sum: Process[Double, Double] =
      def go(acc: Double): Process[Double, Double] =
        Await {
          case Some(d) => Emit(d + acc, go(d + acc))
          case _ => Halt() 
        }
        
      go(0.0)

    def take[I](n: Int): Process[I, I] =  
      
      def go(m: Int): Process[I, I] =
        if m < 0 || m > n then 
          Halt()
        else 
          Await {
            case Some(v) => Emit(v, go(m + 1))
            case _ => Halt()
          }
      go(1)

    def drop[I](n: Int): Process[I, I] =
      def go(m: Int): Process[I, I] =
        if m < 0 then 
          Halt()
        else 
          Await {
            case Some(v) => 
              if m > n then
                Emit(v, go(m))
              else
                go(m + 1)
            case _ => Halt()    
          }
        
      go(1)
      
    def takeWhile[I](f: I => Boolean): Process[I, I] =  
      def go: Process[I, I] =
        Await {
          case Some(v) if f(v) => Emit(v, go)
          case _ => Halt() 
        }
      go

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      def go(continue: Boolean): Process[I, I] =
        Await {
          case Some(v) =>
            if continue && f(v) then 
              go(continue)
            else
              Emit(v, go(false))
          case _ => Halt()
        }
      go(true)
    
    def count[I]: Process[I, Int] =  
      def go(index: Int): Process[I, Int] =
        if index < 0 then 
          Halt()
        else
          Await {
            case Some(_) => Emit(index, go(index + 1))
            case _ => Halt() 
          }

      go(1)
    
    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      Await {
        case Some(i) => f(i, z) match
          case (o, s2) => Emit(o, loop(s2)(f))
        case _ => Halt()    
      }
      
    def monad[I]: Monad[[X] =>> Process[I, X]] = new Monad[[X] =>> Process[I, X]] {
      type TT = [X] =>> Process[I, X]
      override def unit[O](v: => O): TT[O] = Emit(v, Halt())

      extension [O, O2](fa: TT[O])
        def flatMap(f: O => TT[O2]): TT[O2] = fa flatMap f 
 
    }
      
    
    def mean: Process[Double, Double] =
      loop((0.0, 0)) { case (i, s) =>
        val (sum, length) = s 
        val sum2 = sum + i 
        val lenght2 = length + 1
        
        val r = 
          if lenght2 > 0 then 
            sum2 / lenght2
          else 
            0.0
            
        (r, (sum2, lenght2))
      }
      
    // mean的另一种实现的模式
    def mean2: Process[Double, Double] =
      zip(sum, count).map((s, c) => s / c)
      
    def exists[I](f: I => Boolean): Process[I, Boolean] =
      Await {
        case p@Some(v) =>
          if f(v) then 
            Emit(true, Halt())
          else
            exists(f)  
        case _ => Halt()
      }
      
    def exists2[I](f: I => Boolean): Process[I, Boolean] =
      Await {
        case p@Some(v) =>
          if f(v) then 
            Emit(true, Halt())
          else
            Emit(false, exists2(f))
        case _ => Halt()  
      }

    def exists3[I](f: I => Boolean): Process[I, Boolean] =
      Await {
        case p@Some(v) =>
          if f(v) then 
            Emit(true, exists3(f))
          else 
            Emit(false, exists3(f))

        case _ => Halt()  
      }
      
    // Emits whether a `true` input has been received 
    def any: Process[Boolean, Boolean] =
      loop(false)((b, s) => (s || b, s || b))
      
    // We choose to emit all intermediate values, and not halt.
    def exists4[I](f: I => Boolean): Process[I, Boolean] =
      lift(f) |> any
      
    // Like `takeWhile`, but includes the first element that tests false
    def takeThrough[I](f: I => Boolean): Process[I, I] =
      takeWhile(f) ++ echo 
      
    // Awaits then emits a single value, then halsts
    def echo[I]: Process[I, I] = 
      liftOne((i: I) => i)
      
    def existsResult[I](f: I => Boolean): Process[I, Boolean] =
      exists4(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))
      
    def exist5[I](f: I => Boolean): Process[I, Boolean] =
      exists4(f) |> takeThrough[Boolean](e => !e)  


    def feed[I, O](oa: Option[I])(p: Process[I, O]): Process[I, O] =
      p match
        case pp@Halt() => pp
        case Emit(h, t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa) 
        
    // refer
    // https://github.com/fpinscala/fpinscala/blob/master/answerkey/streamingio/07.answer.scala
    def zip[I, O, O2](p1: Process[I, O], p2: Process[I, O2]): Process[I, (O, O2)] = (p1, p2) match
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(v1, t1), Emit(v2, t2)) => Emit((v1, v2), zip(t1, t2))
      case (Await(recv1), pp@_) => Await(option_e => zip(recv1(option_e), feed(option_e)(pp))) // 很显然,需要输入数据里流
      case (pp@_, Await(recv2)) => Await(option_e => zip(feed(option_e)(pp), recv2(option_e))) // 需要输入数据流
      
    def processFile[A, B](s: Source, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
        @annotation.tailrec
        def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
          cur match
            case Halt() => acc
            case Emit(v, t) => go(ss, t, g(acc, v))
            case Await(recv) =>
              val next =
                if ss.hasNext then
                  recv(Some(ss.next))
                else
                  recv(None)
              go(ss, next, acc)
        try
          go(s.getLines(), p, z)
        finally
          s.close
    }
 
  
    def convertFahrenheit: Process[String, String] =
      filter((line: String) => !line.startsWith("#")) |> filter(line => line.trim.nonEmpty) |>
        lift(line => fahrenheitToCelsius(line.toDouble).toString)

    def test(): Unit = {
      val p = liftOne((x: Int) => x * 2)
      val xs = p(LazyList(1, 2, 3)).toList
      println(f"xs is $xs")
      val p2 = lift((x: Int) => x * 2)
      val xs2 = p2(LazyList(1, 2, 3)).toList 
      println(f"xs2 is $xs2")
      
      val even = filter((x: Int) => x % 2 == 0)
      val evens = even(LazyList(1, 2, 3, 4)).toList
      println(f"evens is $evens")
      
      val r2 = sum(LazyList(3, 4, 5)).toList 
      println(f"r2 sum is $r2")
      
      val r3 = take(4)(LazyList(4,5, 6 ,6,7, 8)).toList
      println(f"r3 is $r3")

      val r4 = drop(4)(LazyList(4, 5, 6 ,6 ,7, 8)).toList
      println(f"r4 is $r4")

      val r5 = takeWhile((i: Int) => i % 2 == 0)(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r5 is $r5")

      val r6 = dropWhile((i: Int) => i % 2 == 0)(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r6 is $r6")

      val r7 = count(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r7 is $r7")

      val r8 = mean(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r8 main is $r8")

      val r9 = mean2(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r9 main is $r9")
      
    }
  
    def test2(): Unit = {
      val r = filter[Int](_ % 2 == 0) |> lift(_ + 1)
      val r2 = r(LazyList(3, 4, 5, 6)).toList
      println(f"r2 is $r2")

      val p = lift((x: Int) => x * 2).map( _ / 2 )
      val xs = p(LazyList(1, 2, 3)).toList
      println(f"xs2 is $xs")
    }
  
    def test3(): Unit = {
      val r = lift((n: Int) => n) 
      val r2 = r.map(_ * 2) |> r.map( _ / 2)
      val r3 = r2(LazyList(1, 2, 3)).toList 
      println(f"r3 is $r3")
      
      val r4 = r2.zipWithIndex(LazyList(3, 4, 55)).toList 
      println(f"r4 is $r4")

      val r9 = mean2(LazyList(4, 5, 6 ,6, 7, 8)).toList
      println(f"r9 main is $r9")
      
      val r10 = exists((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r10 is $r10")

      val r11 = exists2((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r11 is $r11")

      val r12 = exists3((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r12 is $r12")

      val r13 = exists4((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r13 is $r13")

      val r14 = existsResult((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r14 is $r14")


      val r15 = exist5((e: Int) => e % 2 == 0)(LazyList(1, 3, 5, 6, 7)).toList
      println(f"r15 is $r15")

       
    }
  
    def test4(): Unit = {
      val r = processFile(Source.fromFile("src/resources/mm.txt"), count |> existsResult(_ > 2), false)(_ || _)
      println(f"r is $r")

      val r2 = processFile(Source.fromFile("src/resources/mm.txt"), convertFahrenheit, List.empty[String])(_ :+ _).run
      println(f"r2 is $r2")
    }
    
 
  
  @main def stream_processing_and_incremental_io_start(): Unit = {
//    Process.test()
//    Process.test2()
//    Process.test3()
    Process.test4()
  }

}
