package scala3_episode.typeclass.aplicative

import scala3_episode.typeclass.functor.functor.Functor

object applicative {
  
  trait Applicative[F[_]] extends Functor[F]:
    
    extension [A, B](fa: F[A])
      def ap(fab: F[A => B]): F[B]
  
      
  object Applicative:
    def apply[F[_]](using ap: Applicative[F]): Applicative[F] = ap 
}
