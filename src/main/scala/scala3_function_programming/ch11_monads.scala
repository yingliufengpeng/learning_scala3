package scala3_function_programming

import scala3_function_programming.ch08_property_based_testing.Gen
import scala3_function_programming.ch06_general_state.{State}

object ch11_monads {
  
  trait Functor[F[_]]:
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
      
    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
        
    extension [A, B](fa: F[A])
      def map(f: A => B): F[B] // 这个只是代表着函数签名的写法
  
  object Functor:
    val r = 110
  
    given Functor[List] = new Functor[List] {
      extension [A, B](fa: List[A])
        def map(f: A => B): List[B] = fa map f
    }
  
  case class Id[A](value: A):
    def map[B](f: A => B): Id[B] =
      Id(f(value))
    def flatMap[B](f: A => Id[B]) =
      f(value)
      
  
  case class Reader[R, A](run: R => A):
    self =>
    def map[B](f: A => B): Reader[R, B] =
      flatMap(_ => Reader(r => f(self.run(r))) )
      
    def flatMap[B](f: A => Reader[R, B]) =
      Reader {
        (r: R) =>
          val a = self.run(r)
          val reader = f(a)
          reader.run(r)
      }
  
  trait Monad[F[_]] extends Functor[F]:
    def unit[A](v: => A): F[A] 
    
    def seuqence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldLeft(unit(List.empty[A]))((acc, fa) => map2(fa, acc)(_ :: _))
      
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = 
      seuqence(la map f)
      
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
      seuqence(List.fill(n)(ma))
     
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =  
      ms.map(e => (e, f(e))).foldLeft(unit(List.empty[A]))(
        (acc, t) => 
         map2(acc, t._2)((acc1, v1) =>
            if v1 then 
                t._1 :: acc1
            else 
                acc1
          ) 
        )
 
    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = 
      map2(ma, mb)((_, _))
      
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      for 
        a <- fa 
        b <- fb
      yield 
        f(a, b)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a =>
        f(a).flatMap(g)
        
    extension [A, B](fa: F[A])
      def map(f: A => B): F[B] =
        flatMap(e => unit(f(e)))
        
      // 需要打破compose和flatMap之间的循环的桥梁
//      def flatMap(f: A => F[B]): F[B] =  
//        compose((a: Int) => fa, (a: A) => f(a))(0)
      def flatMap(f: A => F[B]): F[B] 

      def join(mma: F[F[A]]): F[A] =
        mma.flatMap(e => e)
  
  object Monad:
    given Monad[Gen] = new Monad[Gen] {
      override def unit[A](v: => A): Gen[A] = Gen.unit(v)
      
      extension [A, B](fa: Gen[A])
        def flatMap(f: A => Gen[B]): Gen[B] =
          fa flatMap f
    }

    given Monad[Option] = new Monad[Option] {
      override def unit[A](v: => A): Option[A] = Some(v)
      
      extension [A, B](fa: Option[A])
        def flatMap(f: A => Option[B]): Option[B] =
          fa flatMap f
    }

    given Monad[Id] = new Monad[Id] {
      override def unit[A](v: => A): Id[A] = Id(v)

      extension [A, B](fa: Id[A])
        def flatMap(f: A => Id[B]): Id[B] =
          fa flatMap f

    }

    implicit def stateMonad[S]: Monad[[A] =>> State[S, A]] = new Monad[[A] =>> State[S, A]] {
      type ST = [A] =>> State[S, A]
      def unit[A](a: => A): State[S,A] = State(s => (a, s))

      extension [A, B](fa: ST[A])
        def flatMap(f: A => ST[B]): ST[B] =
          fa flatMap f
 
    }
    
    given Monad[[A] =>> State[Int, A]] = new Monad[[A] =>> State[Int, A]] {
      type ST = [AA] =>> State[Int, AA]
      override def unit[A](v: => A): State[Int, A] =
        State.run(s => (v, s))

      extension [A, B](fa: ST[A])
        def flatMap(f: A => ST[B]): ST[B] =
          fa flatMap f
     }

    implicit def readerMonad[R]: Monad[[A] =>> Reader[R, A]] = new Monad[[A] =>> Reader[R, A]] {
      type Re = [A] =>> Reader[R, A]
      def unit[A](a: => A): Reader[R,A] = Reader(r => a)

      extension [A, B](fa: Re[A])
        def flatMap(f: A => Re[B]): Re[B] =
          fa flatMap f
       
    }

 
    
    
    case class Item(name: String, price: Double)
    case class Order(item: Item, quantity: Int)
    
    def test3(): Unit = {
      val F = summon[Monad[[A] =>> State[Int, A]]]
      def zipWithIndex[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit( List.empty[(Int, A)]  ))( (acc, a) =>
          for 
            xs <- acc 
            n <- acc.get
            _ <- acc.set(n + 1)
          yield 
            (n, a) :: xs
        ).run(0)._1.reverse
        
      val r = zipWithIndex(List(3, 4, 5, 6))
      println(f"r is $r")
    }
    
    def test2(): Unit = {
      
      val m = summon[Monad[Id]]
      val r = m.unit(3)
      val r2 = m.replicateM(10, r)
      println(f"r2 is $r2")
      
//      val m2 = summon[Monad[Reader[Int, Int]]]
      
      
    }
    
    def test(): Unit = {
      val genOrder: Gen[Order] =  
        for 
          name <- Gen.stringN(3)
          price <- Gen.double 
          quantity <- Gen.choose(1, 100)
        yield 
          Order(Item(name, price), quantity)
          
      val genItem: Gen[Item] = 
        for 
          name <- Gen.stringN(3)
          price <- Gen.double
        yield 
          Item(name, price)
          
      val genOrder2: Gen[Order] = 
        for 
          item <- genItem
          quatity <- Gen.choose(1, 100)
        yield 
          Order(item, quatity)
      


    }
  
  trait MonadCatch[F[_]] extends Monad[F]:
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](f: Throwable): F[A]
  
  
  @main def monads_start(): Unit = {
    Monad.test()
    Monad.test2()
    Monad.test3()
  }

}
