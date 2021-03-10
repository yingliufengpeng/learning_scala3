package scala3_tools_demo

object application {
  
  def f[A, B]: B = ???  
  
  val e1: Either[String, Int] = Right(10)
  val e2: Either[String, Int] = Left("Oops")
  
  trait Functor[F[_]]:
    extension [A, B] (fa: F[A])
      def fmap(f: A => B): F[B]
      def map(f: A => B): F[B] = fa fmap f 
  
  object Functor:
    
    def apply[F[_]](using f: Functor[F]): Functor[F] = f 
  
    given eitherFunctor[Err]: Functor[[X] =>> Either[Err, X]] with  
        extension [A, B] (fa: Either[Err, A])
          override def fmap(f: A => B): Either[Err, B] = fa map f 
    
    
    given Functor[List] with 
      extension [A, B] (fa: List[A])
        override def fmap(f: A => B): List[B] = fa map f 
  
    
    def test(): Unit = {
      val r = e1 fmap (e => e * 100)
      println(s"r is $r")
      
      val f = (v: Int) => v + 1
      val g = (v: Int) => v - 1
      
      val r2 = List(1, 2, 4)
      val r3 = r2.map(f).map(g) == r2.map(f andThen g)
      
      println(s"r3 is $r3")
    }
  
  trait Applicative[F[_]] extends Functor[F]:
    extension [A, B] (fa: F[A])
      def ap(fab: F[A => B]): F[B]
  
    extension [A, B, C] (fa: F[A])
      def map2(fb: F[B])(f: (A, B) => C): F[C] =
        val fbc: F[B => C] = fa.fmap((a: A) => (b: B) => f(a, b))
        fb.ap(fbc)
  
  
  trait Monad[F[_]] extends Applicative[F]:
    def pure[A](x: A): F[A]
    
    extension [A, B] (fa: F[A])
      def fflatMap(f: A => F[B]): F[B]
      def flatMap(f: A => F[B]): F[B] = fa fflatMap f 
      override def ap(fab: F[A => B]): F[B] =
        fa.fflatMap {
          fa => 
            fab.fflatMap {
              fab =>
                pure(fab(fa))
            }
        }
      override def fmap(f: A => B): F[B] =
        ap(pure(f))
        
  
  object Monad:
    
    def apply[F[_]](using f: Monad[F]) = f 
    
    given eitherMonad[Err]: Monad[[X] =>> Either[Err, X]] with 
      override def pure[A](v: A) = Right(v)
      extension [A, B] (fa: Either[Err,  A])
        override def fflatMap(f: A => Either[Err, B]): Either[Err, B] = fa flatMap f
         
    def test(): Unit = {
      val r1 = e1 fmap (_ *2 )
      println(s"r1 is $r1")
    }
  
  def test1(): Unit = {
    val r = e1.map(_ * 20)
    println(s"r is $r")
    
    val r2 = e2.map(_ * 20)
    println(s"r2 is $r2")
    
 
    
  }
  
  @main def application_start(): Unit = { 
    test1()
    Functor.test()
    Monad.test()
  }

}
