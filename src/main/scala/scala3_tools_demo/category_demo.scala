package scala3_tools_demo

object category_demo {
  val f: Int => Int = _ + 1
  val g: Int => Boolean = n => if (n == 1) true else false 
  val h: Boolean => String = if (_) "Ok" else "Error"
  
  trait Monad1[F[_]]: 
    def unit[A](v: A): F[A]
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
  
    extension [A, B] (fa: F[A])
      // A == F[A]  F[B] === F[A] 
      def flatMap(f: A => F[B]): F[B] =
        compose((fa: F[A]) => fa, a => f(a))(fa)
        
      def map(f: A => B): F[B] =
        flatMap(e => unit(f(e)))
  
  
  object Monad1:
    def apply[F[_]](using m: Monad1[F]) = m 
  
    given optionMonad1: Monad1[Option] with
      override def unit[A](a: A): Option[A] = Option(a)
      override def compose[A, B, C](f: A => Option[B], g: B => Option[C]): A => Option[C] = {
        println(s"compose ...")
        a =>
          f(a).flatMap(g)
      }


    def test(): Unit = {
      def f(n: Int): Option[Int] = if n == 4 then None else Some(n)
      def g(n: Int): Option[Boolean] = if n % 2 == 1 then Some(true) else Some(false)
      def h(b: Boolean): Option[String] = if b then Some("Winnter!") else None 
      
      val opMonad1 = Monad1[Option]
      
      val h2 = opMonad1.compose(f, g)
      val r = h2(11)
      println(s"r is $r")
      
      val h3 = opMonad1.compose(opMonad1.compose(f, g), h)
      val h4 = opMonad1.compose(f, opMonad1.compose(g, h))
     
      
      (1 to 10).foreach { e =>
        val r3 = h3(e)
        val r4 = h4(e)
        println(r3 == r4)
      }
    }
  
  
    def test2(): Unit = {
      val r = Option(43)
      val r2 = Option(44)
      val m = Monad1.apply[Option]
      val r3 = m.unit(4)
      println(s"r3 is $r3")
      
    }
    
    
  
  @main def categoy_demo_start(): Unit = {
    Monad1.test()
    Monad1.test2()
  }

}
