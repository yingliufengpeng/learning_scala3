package scala3_tools_demo

import java.io.File
import scala.io.Source

object demo3 {
  
  def target(file: Source): Source =
    ???
    
  
  trait A[T] { def f: A[T]}
  trait B[T] { def f: B[T]}
  
  trait Fun_Run:
    def run() : Unit = println("fun_run")
  
  object Fun_Run:
    def apply(): Fun_Run = new Fun_Run { }
  
  class D[T] extends A[T], B[T]:
    
    override def f: A[T] & B[T] = this

    override def hashCode(): Int = 3333

    override def toString: String = s"A[T] & B[T]"
  
     
  
  @main def demo3_start(): Unit =  {
//    val tmp = target(Source.fromString("kk"))
    val r = D[Int]()
    println(s"r is $r")
    
    val r2 = D[Int]()
    println(r == r2)
    
    val r3 = r2.hashCode()
    println(s"r3 is $r3")
    
    Fun_Run().run()
  }

}
