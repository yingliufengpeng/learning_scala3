package scala3_new_syntax_demo
 
import scala.compiletime.uninitialized
import reflect.Selectable.reflectiveSelectable

object new_syntax_demo {
  
  trait Show2[A]:
    def show(x: A): String 
  
  def f2(elem: Show2[?]): Unit =
    elem.show(???)
  
  val xs: List[Any] = List(1, 2, 3)
  
  val xs2: List[(Int, Int)] = List((43, 4), (6, 6))
  val ys: List[(Int, Int)] = xs2.map((x, y) => (y, x))
  
  extension [A] (xs: List[A])
    def sumBy[B](f: A => B)(using Numeric[B]): B = ???
  
  case class Foo(x: Int)
  
  def f(g: Int => Foo) = g(10)
  
  def f2(using i: Int): Unit = 
    def g(using j: Int): Unit =
      val m: Int = summon[Int]
      println(f"m is $m")
  
    g(using 4)
  
  type Request[A] = {
    type T = A 
  }
  
  type T = {}
  
  type W = {
      val m: Int 
      def Show(): Unit
  }
  
  def show(w: W): Unit = w.Show()
  
  def test3(): Unit = {
    val r: Request[Int] = new {type T = Int }
    println(f"r is $r")
    
    val r2: Request[Int] = new {type T = Int }
    println(f"r2.type is ${r2.getClass()}")
    
    val r3: T = new {}
    println(f"r3 is $r3")
    
    val r4: W = new {
      val m: Int = 4
      def Show(): Unit = println("ok")
    }
    r4.Show()
    show(r4)
  }
  
  
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
    test3()
  }

}
