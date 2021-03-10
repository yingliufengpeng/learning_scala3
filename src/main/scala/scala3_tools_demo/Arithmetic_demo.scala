package scala3_tools_demo

import scala.collection.immutable.Stream.Empty

object Arithmetic_demo {
  enum Expression[T]:
    
    case E_Value(v: T)
  
    case E_Boolean(v: Expression[T])
    case E_Add(left: Expression[T], right: Expression[T])
    case E_Mul(left: Expression[T], right: Expression[T])
    case E_Sub(left: Expression[T], right: Expression[T])
    case E_Expr(parent: Expression[T])
     
    def eval: T =
      ???
    
  
  object Expression:
    
    def single[T](v: T) = E_Value(v)
    
    def add[T](l: Expression[T], r: Expression[T]): Expression[T] =
      E_Add(l, r)
    
    def mul[T](l: Expression[T], r: Expression[T]): Expression[T] =
      E_Mul(l, r)
      
    def sub[T](l: Expression[T], r: Expression[T]): Expression[T] =
      E_Sub(l, r)
      
    //     
    def fromTs[A](arr: A*): Expression[A] = 
      if arr.isEmpty then throw Exception("kkk")
      else 
        arr.tail.foldLeft(single(arr.head))((acc, e) => mul(acc, single(e)))
    

  def test2(): Unit = {
    inline val N = 4
    val r = for 
      i <- 1 to N 
      j <- 1 to N 
    yield 
      i + j 
    
    println(r)
    
  }

  def test(): Unit = {
    val r = 10
    r match
      case 1 => 1
      case 2 => 2
      case 3 | 4 | 5 => 4
      case _ => 0
    
  }   
    
  
  @main def arithmetic_demo_start(): Unit = {
    val r = Expression.fromTs(1, 2, 3)
    println(f"r is $r")
    
    test2()
  }

}
