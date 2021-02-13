package scala3_function_programming
import scala.io.StdIn
import scala3_function_programming.ch12_applicative_and_traversable_functors.Monad

object ch13_external_effects_and_io {
  
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0
  
  // It is always possible to factor an impure procedure into a 
  // pure "core" function and two procedures with side effects: 
  // one that supplies the pure function's input and one that 
  // does something with the pure function's output. In listing 
  // 13.1, we factored the pure function winner out of contest. 
  // Conceptually
  case class Player(name: String, score: Int)
  
  def winner(p1: Player, p2: Player): Option[Player] =
    if p1.score > p2.score then
      Some(p1)
    else if p1.score < p2.score then 
      Some(p2)
    else
      None 
      
  def winnerMsg(p: Option[Player]): String =
    p.map {
      case Player(name, _) => s"$name is the winner!"
    }.getOrElse("It'a a draw")
  
  def contest(p1: Player, p2: Player): IO[Unit] =
    IO.printLine(winnerMsg(winner(p1, p2)))
    
  
  trait IO[+A]:
    self =>
    def run: A 
    
    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B =
        f(self.run)
    }
    
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B =
        f(self.run).run 
    }
    
    def ++[B >: A](io: IO[B]): IO[B] = new IO[B] {
      override def run: B = 
        self.run
        run 
       
    }
  
  object IO extends Monad[IO]:

    override def unit[A](a: => A): IO[A] = new IO[A] {def run: A = a}
    
    def apply[A](a: => A): IO[A] = unit(a)

    override def apply[A, B](fab: IO[A => B])(fa: IO[A]): IO[B] =
      for 
        f <- fab
        a <- fa
      yield 
        f(a)  

    def empty: IO[Unit] = new IO {
      override def run: Unit = ()
    }
  
    def readLine: IO[String] = IO { StdIn.readLine }
  
    def printLine(msg: String): IO[Unit] = IO { println(msg) }
  
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
      
//      val r4 = readInt.forever.run  
      
    }
  
  
  @main def external_effects_start(): Unit = {
    
    
    IO.test()
  }

}
