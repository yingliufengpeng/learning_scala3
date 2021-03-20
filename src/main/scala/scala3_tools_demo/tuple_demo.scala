package scala3_tools_demo
import scala.deriving.Mirror

object tuple_demo {
  
  case class Employee(name: String, number: Int, manager: Boolean)
  
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  
  enum Tup:
    case Empt
    case Tcons[H, T <: Tup](head: H, tail: Tup)
  
  import Tup.*
  extension [A, T <: Tup] (a: A)
    def *: (t: T): Tcons[A, T] = Tcons(a, t) 
  
  type Concat[L <: Tup, R <: Tup] <: Tup = L match 
    case Empt.type => R 
    case Tcons[headT, tailT] => Tcons[headT, Concat[tailT, R]]
  
  def concat[L <: Tup, R <: Tup](left: L, right: R): Concat[L, R] = left match
    case _: Empt.type => right
    case cons: Tcons[_, _] => Tcons(cons.head, concat(cons.tail, right))
  
  def wrong[L <: Tup, R <: Tup](left: L, right: R) : Concat[L, R] = ???
  
  enum Expr[A]:
    case Var(name: String)
    case Apply[A, B](fun: Expr[B => A], arg: Expr[B]) extends Expr[B]
  
    // f [B] 是类型参数的使用的条例
    def mapSubexpressions(f: [B] => Expr[B] => Expr[B]): Expr[A] = this match
      case Var(v) => Var(v)
      case Apply(fun, arg) => Apply(f(fun), f(arg))
  
  def test4(): Unit = {
    import Expr.* 
    val e0 = Apply(Var("f"), Var("a"))
    
    val e1 = e0.mapSubexpressions([B] => (se: Expr[B]) => Apply(Var("wrap"), se))
    
    println(s"e0 is $e0")
    println(s"e1 is $e1")
  }
  
  
  def test3(): Unit = {
    val r = "4" *: 'c' *:  1 *: Empt
    println(s"r is $r")
  }
  
  
  def test2(): Unit = {
    val r = (1, "2", 3, "hello, World") 
    println(s"r is $r")
    
    
    val r2 = r.map[[X] =>> Option[X]]( [T] => (t: T) => Some(t) )
    println(s"r2 is $r2")
    
    val r3 = r.map([T] => (t: T) => List(t))
    println(s"r3 is $r3")
    
    val r4 = r.map([T] => (t: T) => Set(t))
    println(s"r4 is $r4")
  }
  
  
  trait MyList[Elem] 
  
  trait MyList2 { type Elem }
  
  trait MyList3 { type MyList3$Elem }
  
  class ImMylist3 extends MyList3:
    type IMMylist3$T
    type MyList3$Elem = IMMylist3$T 
  
  // Applicatioln such as ImMylist3[String] is expanded to ImMylist3 { type IMMylist3$T = String }  
  
  
  
  def test(): Unit = {
    val bob: Employee = Employee("Bob", 42, false)
    val bobTuple: (String, Int, Boolean) = Tuple.fromProductTyped(bob)
    println(s"bobTuple is $bobTuple")
    val bobAgain: Employee = summon[Mirror.Of[Employee]].fromProduct(bobTuple)
    println(s"bobAgain is $bobAgain")
  }
  
  @main def tuple_demo_start(): Unit = {
    test()
    test2()
    test3()
    test4()
  }
}
