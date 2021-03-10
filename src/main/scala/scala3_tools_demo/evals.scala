package scala3_tools_demo

import scala3_tools_demo.type_class_demo.Numeric

object evals {
  type Env[T] = Map[String, T]
  type WithEnv[T] = Env[T] ?=> T
  
  def summonEnv[T]: Env[T] ?=> Env[T] = summon[Env[T]]
   
  enum Expr[T]:
    case Val(value: T) 
    case Add(left: Expr[T], right: Expr[T])
    case Mul(left: Expr[T], right: Expr[T])
    case Var(identifier: String)
    
   
  
  object Expr:
//    implicit def expr2NumbericExpr[T](using t: Numeric[T]): Numeric[Expr[T]] = new Numeric[Expr[T]] {
//      override def zero: Expr[T] = Val(t.zero)
//      extension (fa: Expr[T])
//        def add(fb: Expr[T]): Expr[T] = fa + fb
//        def mul(fb: Expr[T]): Expr[T] = fa * fb 
//    }
    
    def eval[T](expr: Expr[T])(using env: Env[T],  numeric: Numeric[T]): T = expr match
      case Val(v) => v
      case Var(id) => handleVar(id)
      case Add(l, r) => handleAdd(l, r)
      case Mul(l, r) => handleMul(l, r)
    
    def handleAdd[T](l: Expr[T], r: Expr[T])(using env: Env[T],  numeric: Numeric[T]): T = eval(l) + eval(r)
    def handleMul[T](l: Expr[T], r: Expr[T])(using env: Env[T],  numeric: Numeric[T]): T = eval(l) * eval(r)
    def handleVar[T](identifier: String)(using env: Env[T],  numeric: Numeric[T]): T = 
      env.getOrElse(identifier, numeric.zero)
    
  
  given env: Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)
  
   
  @main def evals_start(): Unit = {
    import Expr._ 
    val expr1: Expr[Int] =
      Mul(
        Var("z"),
        Add(
          Val(30),
          Mul(
            Var("x"),
            Var("y")
          )
        )
      )
    end expr1 
    
    val r = Expr.eval(expr1)
    println(s"r is $r")
    
  }
}
