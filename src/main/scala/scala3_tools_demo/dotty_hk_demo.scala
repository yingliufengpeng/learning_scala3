package scala3_tools_demo

object dotty_hk_demo {
  type Histogram = [T] =>> Map[T, Int]
  type White = Map[_, Int]
  
  abstract class A[T]:
    def v: T 
  
  class AA extends A[Int]:
    val v: Int = 3
  
  
  type Rep[T] = T 
  
  type Foo = [X] =>> X 
  
  
  
  @main def dotty_hk_demo_start(): Unit = {
    val r: Histogram[String] = (1 to 10).map(e => (e.toString, e)).toMap 
    val r2: White = (1 to 10).map(e => (e.toString, e)).toMap 
    
    println(s"r is $r, r2 is $r2")
    
    val r3: Rep[Int] = 3
    
    val r4 = (1 to 1000000).collect { case e => e.toString}.mkString("", ",", "")
    println(r4)
    
    
  }

}
