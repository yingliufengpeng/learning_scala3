package scala3_episode

object eval_result {
  type Env = Map[String, Int]
  enum Exp:
    case Val(value: Int) 
    case Add(left: Exp, right: Exp)
    case Mul(left: Exp, right: Exp)
    case Var(identifer: String)
  
    import Exp.*
    def eval(using env: Env): Int = this match
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l, r) => handleAdd(l, r)
      case Mul(l, r) => handleMul(l, r )
  
  object Exp:
    def handleAdd(l: Exp, r: Exp)(using env: Env): Int = l.eval + r.eval 
    def handleMul(l: Exp, r: Exp)(using env: Env): Int = l.eval * r.eval 
    def handleVar(id: String)(using env: Env): Int = env.getOrElse(id, 0)
    
    def summonEnv: Env ?=> Env = summon[Env]
    
    def test(): Unit ={
      val exp1: Exp =
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
      
      given env: Env = Map("x" -> 17, "y" -> 10, "z" -> 2) 
      
      val eval1 = exp1.eval
      
      print(s"Eval exp gives $eval1")
    } 

  @main def eval_result_start(): Unit = {
    Exp.test()
  }
}
