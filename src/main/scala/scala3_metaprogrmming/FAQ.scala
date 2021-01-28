package scala3_metaprogrmming
import scala.quoted._ 
object FAQ {
  
  // Which shoud I use Expr(...) or '{...}? 
  // If your can write your code using  Expr(...) , you will evaluate more tat 
  // compile time. Only use '{...} if you really need to evaluate the code
  // later at runtime, usually because it depends on runtime values.
  
  
  // Which is better between Expr(true) or '{true}?
  // All quotes containing a value of primiate type is optimised to an 
  // Expr.apply. Choose one in your project and stick with a single
  // notation to avoid confusion
  
  // How do I get a value out of an Expr?
  // If the expression represents a value, you can use .unlift, .unliftOrError
  // , Unlifted.unapply or Const.unapply
  
  // How can i get the precise type of an Expr?
  // We can get the precise type(Type) of an Expr using the following pattern
  // match
  
  @main def FAQ_start(): Unit = {
//     val x: Expr[Int] = ??? 
//     x match
//       case '{ $x: $t } => 
          // `x: Expr[X & t.T]` where `t` is the precise type of `x`
  }

}
