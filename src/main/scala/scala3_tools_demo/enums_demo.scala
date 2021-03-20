package scala3_tools_demo

import jdk.internal.org.objectweb.asm.ByteVector

import scala.quoted._ 

object enums_demo {
  
  def test(): Unit = {
    val rs = List(1, 3, 4, 5)
    val rs1 = rs.dropRight(1)
    val rs2 = rs.dropRight(2)
    println(s"rs2 is $rs2")
  }
  
  class A:
    type T 
    type Coll 
    def show(v: T): Unit = {
      println(v)
    }
  
  
  type TupleMap[T <: Tuple, F[_]] <: Tuple = T match 
    case EmptyTuple => EmptyTuple
    case h *: t => F[h] *: TupleMap[t, F]

  type InverseMap[T <: Tuple, F[_]] <: Tuple  = T match 
    case EmptyTuple => EmptyTuple
    case F[h] *: t => h *: InverseMap[t, F]
  
  def sequence[T <: Tuple](t: T): Option[InverseMap[T, Option]] =
    val unwrapped = t.productIterator.collect { case Some(x) => x}.toArray
    if unwrapped.length == t.productArity then 
      Some(Tuple.fromArray(unwrapped).asInstanceOf[InverseMap[T, Option]])
    else 
      None 
      
  extension [T] (x: T) 
    def show = println("kkk")
   
    
  
  def test3(): Unit = {
    val x = "0001111111111333ffff"
    
    val a = 1 *: (true, 3, "h")
    val b = (1, true) ++ (3, "Hi")
    println(s"a is $a")
    println(s"b is $b")
    
  }
  
  def test2(): Unit = {
    val r = (Some(10), Some("4"), Some(true)): TupleMap[(Int, String, Boolean), Option]
    println(s"r is $r")
    
    val r2 = sequence(r)
    println(s"r2 is $r2")
    
     test3()
  }
  
  
  def test1(): Unit = {
    object M:
      class A:
      
        type T = Int 
    
    
    
    val r = 30
    val r2: r.type = r // TermRef(None, r)
    
    val r3: M.A#T = 3  // TypeRef(M.A, T)
    
    def f[A, B <: Ordering[A]](x: A, y: B): Unit = ???
    
  }
  
  @main def enum_demo_start(): Unit = {
    test()
    test1()
    test2()
  }

}
