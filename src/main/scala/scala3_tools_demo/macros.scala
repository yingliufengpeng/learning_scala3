package scala3_tools_demo
import scala.quoted.*

object Macros:


  def test1()(using Quotes): Expr[Int] =
    val expr = '{ 100 + 50 }
    println(s"Evaluating Macro")
    println(s"Type is $expr")
    expr 
//    Expr(333)


  inline def test2(): Int = ${test1()}