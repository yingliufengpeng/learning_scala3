package scala3_decorator

import scala.languageFeature.experimental
import scala.quoted._ 

object macros {
  
  inline def route[T, U](inline route: String, inline control: String)(inline f: T => U): T => U = 
    ${ routeImpl('route, 'control)('f) }
  
  private def routeImpl[T, U](route: Expr[String], control: Expr[String])(exprF: Expr[T => U] )
                             (using Quotes, Type[T], Type[U]): Expr[T => U] = {
     
    '{ 
      val r1 = $route
      val r2 = $control
      (t: T) =>
        val f = $exprF
        println(s"route: $r1 control: $r2")
        f(t)
    }
  }
  
  def t(x: Int, y: Int)(z: Int) =
    val r1 = x 
    val r2 = y 
    val r3 = z 
    r1 + r2 + r3


}


