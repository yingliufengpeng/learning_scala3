package scala3_episode.typeclass.monad

import scala3_episode.typeclass.aplicative.applicative.Applicative
 
object monad {
  
  // Monad 的强大之处是有了pure/unit遮掩的方法来做处理, compose方法和flatMap方法是相辅相成的作用,不过对于flatMap方法来说,这个
  // 则是关注上一个计算逻辑依赖于下一个计算逻辑的思维产物
  trait Monad[F[_]] extends Applicative[F]:
    def pure[A](x: A): F[A]
    
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a =>
        f(a).flatMap(g)
  
    extension [A, B] (fa: F[A])
      def flatMap(f: A => F[B]): F[B] =
        compose(identity[F[A]], f)(fa)
      
      def map(f: A => B): F[B] = fa flatMap (a => pure(f(a)))
  
      def ap(fab: F[A => B]): F[B] =
        for 
          a <- fa 
          f <- fab
        yield 
          f(a)  
          
    extension [A, B, C] (fa: F[A])
      def map2(fb: F[B])(f: (A, B) => C): F[C] =
        for 
          a <- fa 
          b <- fb 
        yield 
          f(a , b)  
  
  object Monad:
    def apply[F[_]](using m: Monad[F]): Monad[F] = m 
    
    given optionMonad: Monad[Option] with 
      def pure[A](x: A) = Some(x)
      extension [A, B] (fa: Option[A])
        override def flatMap(f: A => Option[B]): Option[B] = fa flatMap f 
  
    given eitherMonad[Err]: Monad[[X] =>> Either[Err, X]] with 
      def pure[A](x: A) = Right(x)
      extension [A, B] (fa: Either[Err, A]) 
        override def flatMap(f: A => Either[Err, B]) = fa flatMap f 
  
    def test(): Unit = {
      def f(n: Int): Option[Int] = if n == 4 then None else Some(n)
      def g(n: Int): Option[Boolean] = if n % 2 == 1 then Some(true) else Some(false)
      def h(b: Boolean): Option[String] = if b then Some("Winner") else None
       
      val option_Monad = Monad[Option]
      val r = option_Monad.compose(f, option_Monad.compose(g, h))(41)
      val r2 = option_Monad.compose(option_Monad.compose(f, g), h)(41)
      
      println(s"r is $r r2 is $r2")
      
    }
  
  
  @main def monad_start(): Unit = {
    Monad.test()
  }
}
