package scala3_episode

import scala3_episode.datatypes.writerT.WriterT
import scala3_episode.typeclass.monad.monad.Monad
import scala3_episode.typeclass.numeric.Scala3Numeric.Numeric
import scala3_episode.typeclass.monad.monad.Monad.*
import scala3_episode.typeclass.semigroup.semigroup.Monoid 

object eval_error {
  
  enum EvalError:
    case InvalidSymbolName
    case DivisionByZero
    case SymbolNotFound(id: String)
  
  type EvalResult[A] = Either[EvalError, A]
  type EvalResultW[A] = WriterT[EvalResult, List[String], A]
  
  def mapTell2[A, B, C, F[_]: Monad, W: Monoid](fa: WriterT[F, W, A], fb: WriterT[F, W, B])(fabc: (A, B) => C)(fabcw: (A, B, C) => W): WriterT[F, W, C] = {
    WriterT(fa.wrapped.map2(fb.wrapped) {
      case ((w1, a), (w2, b)) => 
        val c = fabc(a, b)
        val w = fabcw(a, b, c)
        (w1 |+| w2 |+| w, c)
    })
  }


  object EvalError:
    given evalResultNumberic[A: Numeric]: Numeric[EvalResultW[A]] with 
  
      def zero: EvalResultW[A] = WriterT.lift(Right(Numeric[A].zero))
  
      extension (fa: EvalResultW[A])
        def isZero: Boolean = fa.wrapped match
          case Right((_, v)) if v.isZero => true
          case _ => false 
        
        def add (fb: EvalResultW[A]): EvalResultW[A] =
          mapTell2(fa, fb)((a, b) => a + b)((a, b, c) => List(s"$c: added $a to $b"))
           
        def mul (fb: EvalResultW[A]): EvalResultW[A] =
          mapTell2(fa, fb)((a, b) => a * b)((a, b, c) => List(s"$c: multiplied $a by $b"))
          
        def sub (fb: EvalResultW[A]): EvalResultW[A] =
          mapTell2(fa, fb)((a, b) => a - b)((a, b, c) => List(s"$c: subtracted $a from $b"))
      
        def div (fb: EvalResultW[A]): EvalResultW[A] = {
          if fb.isZero then
            WriterT.lift(Left(EvalError.DivisionByZero))
          else
            mapTell2(fa, fb)((a, b) => a / b)((a, b, c) => List(s"$c: divied $a by $b"))
        }


}
