package scala3_episode.datatypes

import scala3_episode.eval_error.EvalError
import scala3_episode.typeclass.functor.functor.Functor
import scala3_episode.typeclass.monad.monad.*
import scala3_episode.typeclass.semigroup.semigroup.Monoid

object writerT {
  
  case class WriterT[F[_], W, A](wrapped: F[(W, A)]):
    def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =
      WriterT(wrapped map ((l2, a) => ((l2 |+| l1), a)))
      
    def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =
      WriterT(wrapped map ((l2, a) => ((l2 |+| faw(a)), a)))
  
  object WriterT:
    def tell[F[_]: Monad, W, A](w: W): WriterT[F, W, W] = WriterT(Monad[F].pure(w) map (w => (w, w)))
    
    def lift[F[_], W, A](fa: F[A])(using m: Monoid[W], f: Functor[F]): WriterT[F, W, A] =  
      WriterT(fa map (a => (m.zero, a)))
       
    extension [F[_]: Monad, W: Monoid, A] (fa: WriterT[F, W, A])
        def show = ???
        def flatMap[B](f: A => WriterT[F, W, B]): WriterT[F, W, B] =
          Monad[[X] =>> WriterT[F, W, X]].flatMap(fa)(f)
          
        def map2[B, C](fb: WriterT[F, W, B])(f: (A, B) => C): WriterT[F, W, C] =
          Monad[[X] =>> WriterT[F, W, X]].map2(fa)(fb)(f)
//      def map[B](f: A => B): WriterT[F, W, B] =
//        Monad[[X] =>> WriterT[F, W, X]].map(fa)(f)
        
 
    given writerTransformerMonad[F[_]: Monad, W: Monoid]: Monad[[X] =>> WriterT[F, W, X]] with 
      type FF[AA] = WriterT[F, W, AA]
      def pure[A](v: A): FF[A] = WriterT.lift(Monad[F].pure(v))
    
      extension [A, B] (fa: FF[A])
        override def flatMap(f: A => FF[B]): FF[B] = 
          WriterT(fa.wrapped.flatMap((w1, a) => f(a).wrapped.map((w2, b) => (w1 |+| w2, b))))
    
    def test2(): Unit = {
      type StringEither[A] = Either[String, A]
      type StringEitherWriter[A] = WriterT[StringEither, List[String], A]
       
      
      def incrementEven(n: Int): StringEither[Int] =
        if n % 2 == 0 then  
          Right(n + 1)
        else
          Left("Not an Even number")  
          
      def doubleOdd(n: Int): StringEither[Int] =
        if n % 2 == 1 then  
          Right(n * 2)
        else  
          Left("Not an odd number")
     
      val m: Monad[StringEitherWriter] = Monad[StringEitherWriter]
      println(s"m is $m")
      val m2: WriterT[StringEither, List[String], Int] = m.pure(4)
      println(s"m2 is $m2")
    
   
      
      val program1: StringEitherWriter[Int] = 
        for 
          a <- Monad[StringEitherWriter].pure(10).tellWith(a => List(s"Initialized with $a"))
          b <- lift[StringEither, List[String], Int](incrementEven(a))
          c <- lift[StringEither, List[String], Int](doubleOdd(b))
        yield 
          c 
      println(s"program1 is $program1")
    }
   

    def test(): Unit = {
      val e1: Either[String, Int] = Right(10)
      
      val we2 = lift[[A1] =>> Either[String, A1], List[String], Int](e1)
      println(s"e2 is $we2")
      
      val we3 = lift[[A1] =>> Either[String, A1], List[String], Int](e1)
      println(s"we3 is $we3")
      
    }
  
    def test3(): Unit = {
      type EvalResult[A] = Either[EvalError, A]
      type EvalResultLogged[A] = WriterT[EvalResult, List[String], A]
      
      def incrementEven(a: Int): EvalResult[Int] =
        if a % 2 == 1 then 
          Left(EvalError.SymbolNotFound("even"))
        else
          Right(a + 1 )  
       
      def doubleOdd(a: Int): EvalResult[Int] =
        if a % 2 == 0 then 
          Left(EvalError.DivisionByZero)
        else  
          Right(a + a)  
          
      val p = Monad[EvalResultLogged].pure(8).flatMap(e => WriterT.lift(doubleOdd(e))).flatMap(e => WriterT.tell(List("Hello, World!!!")))
      println(s"p is $p")
      
      val answer: EvalResultLogged[Int] = {
        for 
          s1 <- Monad[EvalResultLogged].pure(8)
          _  <- WriterT.tell[EvalResult, List[String], Int](List(s"Wang do Something with $s1"))
          s2 <- WriterT.lift[EvalResult, List[String], Int](incrementEven(s1))
          _  <- WriterT.tell[EvalResult, List[String], Int](List(s"Now we have $s2"))
          s3 <- WriterT.lift[EvalResult, List[String], Int](doubleOdd(s2))
        yield 
          s3
      }
      
      println(s"anwser is $answer")
    }
  
  
  @main def writerT_start(): Unit = {
    WriterT.test()
    WriterT.test2()
    WriterT.test3()
  }

}
