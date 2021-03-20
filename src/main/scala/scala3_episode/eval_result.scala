package scala3_episode

import scala3_episode.Scala3Numeric.Numeric

object eval_result {
  
  type Env[T] = Map[String, T]
  
  enum Exp[T]:
    case Val(value: T) 
    case Add(left: Exp[T], right: Exp[T])
    case Mul(left: Exp[T], right: Exp[T])
    case Var(identifer: String)
  
    import Exp.*
    def eval(using env: Env[T], numeric: Numeric[T]): T = this match
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l, r) => handleAdd(l, r)
      case Mul(l, r) => handleMul(l, r)
  
  object Exp:
    def handleAdd[T](l: Exp[T], r: Exp[T])(using env: Env[T], numeric: Numeric[T]): T = l.eval + r.eval 
    def handleMul[T](l: Exp[T], r: Exp[T])(using env: Env[T], numeric: Numeric[T]): T = l.eval * r.eval 
    def handleVar[T](id: String)(using env: Env[T], numeric: Numeric[T]): T = env.getOrElse(id, numeric.zero)
    
    def summonEnv[T]: Env[T] ?=> Env[T] = summon[Env[T]]
    
    def test(): Unit ={
      val exp1: Exp[Int] =
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
      
      given env: Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2) 
      
      val eval1 = exp1.eval
      
      println(s"Eval exp gives $eval1")
    }

    def test2(): Unit ={
      val exp1: Exp[String] =
        Mul(
          Var("z"),
          Add(
            Val("30"),
            Mul(
              Var("x"),
              Var("y")
            )
          )
        )

      given env: Env[String] = Map("x" -> "17", "y" -> "10", "z" -> "2")

      val eval1 = exp1.eval

      println(s"Eval exp gives $eval1")
    }

  @main def eval_result_start(): Unit = {
    Exp.test()
    Exp.test2()
  }
}
