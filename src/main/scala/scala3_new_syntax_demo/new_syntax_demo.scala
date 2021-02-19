package scala3_new_syntax_demo
import scala.compiletime.uninitialized

object new_syntax_demo {
  
  extension [A] (xs: List[A])
    def sumBy[B](f: A => B)(using Numeric[B]): B = ???
  
  case class Foo(x: Int)
  def f(g: Int => Foo) = g(10)
  
  def f2(using i: Int): Unit = 
    def g(using j: Int): Unit =
      val m: Int = summon[Int]
      println(f"m is $m")
  
    g(using 4)
  
  
  def test2(): Unit = {
    f2(using 3)
  }
  
  object A:
    val left = 1
    val right = 2
    val ** = 4
  
  class B[A]:
    var x: A = uninitialized
  
  object B:
    import A.* 
    import A.{** as star}
    val r = left + right 
    
    def count(as: Int*): Int =
      if as.isEmpty then
        0
      else
        count(as.tail *)  
    
    def test(): Unit = {
      print(f"r is $r star is $star")
      val r2 = count(1, 2, 3)
      println(f"r2 is $r2")
      
      val b = B[Int]
      println(f"b'x is ${b.x}")
      
      val r3 = f(Foo(_))
      println(f"r3 is $r3")
       
    }
    
  
  def test1(): Unit = {
    List("a", "bb", "ccc").sumBy[Int](_.length)
    
    sumBy[String](List("a", "bb", "ccc"))(_.length)
    
    sumBy[String](List("a", "bb", "ccc"))[Int](_.length)
  }
  
  @main def new_syntax_demo_start(): Unit = {
    test2()
  }

}
