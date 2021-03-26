package dotty_new.tools.dot.util

import scala.collection.mutable.ListBuffer



enum Statement:
  case IF(expr: Boolean, body: Statement, elifs: List[IF], `else`: IF)
  case While(expr: Boolean, body: Statement)



def test4(): Unit = {
  
}
  

abstract class Node[+T <: Node[T]]:
  def children: Seq[T]

trait Expression extends Node[Expression]

trait Binary extends Node[Expression]:
  def left: Expression
  def right: Expression

  override def children: Seq[Expression] = left :: right :: Nil 
  
  
case class Add(left: Expression, right: Expression) extends Binary
  
case class Location(row: Int, col: Int, `type`: Int)
object Location:
  
  val buf = ListBuffer.empty[Location]
  val buf2 = collection.mutable.Set.empty[Location]
  
  
  def look_up(row: Int, col: Int, tpe: Int): Boolean =
    val tmp = Location(row, col, tpe)
    buf.exists(_ == tmp)
  
  def row(r: Int):Location = Location(r, 0, 0)
  def col(c: Int): Location = Location(0, c, 0)
  def `type`(t: Int): Location = Location(0, 0, t)

type Key = Location 
type Value = Int | Long | String | Map[_, _]
val r: Map[Int, Value] = Map(1 -> "1", 2 -> 3L, 3 -> 3, 4 -> "kk", 5 -> Map("kk" -> "kk"))

def test3(): Unit = {
  val r = "33" match
    case "44" => 44
    case "33" => 33
    case _ => 0
  
  val r2 = r match
    case 44 => "44"
    case 33 => "33"
    case _ => ""
  
  println(s"r3 is $r2")
  
}

def test2333(): Unit = {
  def abs(x: Int): Int = x match
    case p if p > 0 => p
    case pp => -pp 
   
  
  val r = abs(-3)
  println(s"r is $r")
}


def test222(): Unit = {
  
  // Now let's formalize the process in terms of procedures. We start with a value 
  // for the radicand ( the number whose sequare root we are trying to compute ) and 
  // a value for the guess. If the guess is good enough for our purpose, we are done;
  // if not, we must repeat the process with an improved guess. We write this basic
  // strategy as a procedure.
  
  val f1 :: f2 :: ts = List(1, 2, 3)
  println(s"h1 is $f1  h2 is $f2, ts is $ts")
  
  inline val res = true
  
  res match
    case true => ()
    case _    => ()
  
  
  def go(acc: BigInt = 0): BigInt = {
    println(s"acc is $acc")
    if acc < BigInt("33") then
      go(acc + 1)
    else
      acc
  }

  val r2 = go(0)
  println(s"r2 is $r2")
  
  
  val r = s"有杀手级别的应用!!!"
  
  println(r)
  
  // 选择 分支 循环 
  // function programming 两者是相同的情况 
  
  
}


@main def scala3_syntax_demo_start(): Unit = {
  test3()
}


