package scala3_tools_demo

import scala3_tools_demo.application.Functor
import scala3_tools_demo.application.Monad
import scala3_tools_demo.opaque_types.Monoid
 
object transformer_demo {
  
  case class WriterT[F[_], W, A](wrapped: F[(W, A)]):
    def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =  
      val ffa = wrapped.map((l2, a) => (l1 |+| l2, a))
      WriterT(ffa)

    def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =
      val ffa = wrapped.map((l1, a) => (l1 |+| faw(a), a))
      WriterT(ffa)
    


  object WriterT:
    def lift[F[_], W, A](fa: F[A])(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =
      WriterT(fa map (e => (m.zero, e)))
      
    given writerTransformerMonad[F[_]: Monad, W: Monoid]: Monad[[X] =>> WriterT[F, W, X]] with 
//    given writerTransformerMonad[F[_]: Monad, W: Monoid]: Monad[WriterT[F, W, ?]] with 
      def pure[A](v: A): WriterT[F, W, A] = WriterT.lift( Monad[F].pure(v) )
  
      extension [A, B] (fa: WriterT[F, W, A])
        def fflatMap(f: A => WriterT[F, W, B]): WriterT[F, W, B] =
          val ffb2 =
            for 
              t <- fa.wrapped
              (w, a) = t 
              t2 <- f(a).wrapped
              (w2, a2) = t2
            yield 
              ( w |+| w2, a2)
            
          WriterT(ffb2)
        


  def test(): Unit = {
    
    val e1: Either[String, Int] = Right(10)
    
    val wel = WriterT.lift[[X] =>> Either[String, X], List[String], Int](e1)
    
    type StringEither[A] = Either[String, A]
    type StringEitherWriter[A] = WriterT[StringEither, List[String], A]
    
    def incrementEven(n: Int): StringEither[Int] =
      if n % 2 == 0 then Right(n + 1) else Left("Not an even number")

    def doubleOdd(n: Int): StringEither[Int] =
      if n % 2 == 1 then Right(n * 2) else Left("Not an odd number")
      
    val program1 =  
      for 
        a <- Monad[StringEitherWriter].pure(10).tellWith(a => List(s"Initialized with $a"))
        b <- WriterT.lift(incrementEven(a)).tellWith(a => List(s"incremented to $a"))
        c <- WriterT.lift(doubleOdd(b)).tellWith(a => List(s"doubled to $a"))
      yield 
        c
    println(s"program1 is $program1")    
    
//    val p8 = Monad[WriterT[Either, String, ?], String, ?]
      
    
    
  }
  
  @main def transformer_demo_start(): Unit = {
    test()
  }

}
