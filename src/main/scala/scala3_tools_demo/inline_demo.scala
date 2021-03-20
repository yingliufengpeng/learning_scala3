package scala3_tools_demo
import scala.compiletime.{constValue, S}

object inline_demo {
  
  enum Nat2:
    case object Zero  
    case Succ[N <: Nat2](n: N) 
//
  trait Nat
  case object Zero extends Nat
  case class Succ[N <: Nat](n: N) extends Nat
  import Nat2._ 
  
  object Nat:
    transparent inline def toInt[N <: Nat]: Int = inline scala.compiletime.erasedValue[N] match
      case _: Zero.type => 0
      case _: Succ[n] => toInt[n] + 1
  
    def test(): Unit = {
      val r = toInt[Succ[Succ[Zero.type ]]]
      println(s"r is $r")
      
    }
  
  transparent inline def toInt(n: Nat): Int =
    inline n match
      case Zero => 0
      case Succ(n1) => toInt(n1) + 1
      
  object Ints:
    transparent inline def toIntC[N]: Int =
      inline constValue[N] match
        case 0 => 0
        case _: S[n1] => 1 + toIntC[n1]
        
    
        
    
    def test(): Unit = {
      inline val r = toIntC[1]
      println(s"r is $r")
      
    }
  
  @main def inline_demo_start(): Unit = {
    inline val natTwo = toInt(Succ(Succ(Zero)))
    val intTwo: 2 = natTwo
    println(s"natTwo is $intTwo")
    
    Ints.test()
    Nat.test()
  }

}
