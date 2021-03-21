package scala3_episode.typeclass.functor

object functor {

  trait Functor[F[_]]:
    extension[A, B] (fa: F[A])
      def map(f: A => B): F[B]


  object Functor:

    def apply[F[_]](using f: Functor[F]): Functor[F] = f

    given eitherStringFunctor[Err]: Functor[[X] =>> Either[Err, X]] with 
      type F[X] = Either[Err, X]
      extension[A, B] (fa: F[A])
        def map(f: A => B): F[B] = fa map f

      given Functor[List] with
        extension[A, B](fa: List[A])
  
          def map(f: A => B): List[B] = fa map f


    def test(): Unit = {
      val either = summon[Functor[[X] =>> Either[String, X]]]
      println(s"either is $either")

    }


  @main def functor_start(): Unit = {
    Functor.test()
  }

}
