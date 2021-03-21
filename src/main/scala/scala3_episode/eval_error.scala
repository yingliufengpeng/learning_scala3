package scala3_episode

import scala3_episode.typeclass.numeric.Scala3Numeric.Numeric 
import scala3_episode.typeclass.monad.monad.Monad.{given}
 

object eval_error {
  
  enum EvalError:
    case InvalidSymbolName
    case DivisionByZero
    case SymbolNotFound(id: String)
  
  type EvalResult[A] = Either[EvalError, A]
  
  
  object EvalError:
    given evalResultNumberic[A: Numeric]: Numeric[EvalResult[A]] with 
  
      def zero: EvalResult[A] = Right(Numeric[A].zero)
  
      extension (fa: EvalResult[A])
        def isZero: Boolean = fa match
          case Right(v) if v.isZero => true
          case _ => false 
        def add (fb: EvalResult[A]): EvalResult[A] =
          (fa map2 fb) ((a,b) => a + b)  
        def mul (fb: EvalResult[A]): EvalResult[A] =
          (fa map2 fb)(_ * _)
        def sub (fb: EvalResult[A]): EvalResult[A] =
          (fa map2 fb) ((a,b) => a - b)
      
        def div (fb: EvalResult[A]): EvalResult[A] = {
          if fb.isZero then
            Left(EvalError.DivisionByZero)
          else
            (fa map2 fb) ((a,b) => a / b)
        }


}
