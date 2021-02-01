package scala3_metaprogrmming
import scala.quoted._
import scala.quoted.runtime.impl.QuotesImpl 
import dotty.tools.dotc.core.Constants

object reflection {
  object Macro:
    inline def natConst(inline x: Int): Int = ${ natConstImpl('{x}) }
  
    def natConstImpl(x: Expr[Int])(using Quotes): Expr[Int] =
      import quotes.reflect._ 
      val tree: Term = x.asTerm 
      tree match
        case Inlined(_, _, Literal(IntConstant(n))) => ???
          if n <= 0 then  
            report.error(f"Parameter must be natrual number")
            '{0}
          else
            tree.asExprOf[Int]
        case _ =>
          report.error("Parameter must be a known constant")
          Expr(0)
  
  def macroImpl()(using qtutes: Quotes): Expr[Unit] =
    import quotes.reflect._ 
    val pos = Position.ofMacroExpansion 
    val path = pos.sourceFile.jpath.toString 
    val start = pos.start 
    val ennd = pos.end 
    val startColumn = pos.startColumn 
    val endColumn = pos.endColumn 
    val sourceCode = pos.sourceCode
    ???
  
//  def collectPatternVariables(tree: Tree)(using ctx: Context): List[Symbol]:
//    ???
  
  
    
 
  @main def reflection_start(): Unit = {
   
  }

}
