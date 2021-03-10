package scala3_function_programming

import scala3_function_programming.ch10_monoids.{Foldable, Monoid}
import scala3_function_programming.ch11_monads.Functor
import scala3_function_programming.ch06_general_state.{State}

import java.util.Date

object ch12_applicative_and_traversable_functors {
  
  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  trait Applicative2[F[_]] extends Functor[F]:
    // primitigve combinators
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  
    def unit[A](a: => A): F[A]
    
    // derived combinators
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]]
      = as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))
    
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(fa => fa.map(identity))
      
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))
      
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))
  
    extension [A, B](fa: F[A])
      def map(f: A => B): F[B] =
        map2(fa, unit(()))((a, _) => f(a))
 
  trait Applicative[F[_]] extends Functor[F]:
    self =>
    // primitigve combinators
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)(_(_))

    def unit[A](a: => A): F[A]

    // define in apply and unit
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)
      
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[A,B,C,D,E](fa: F[A],
                        fb: F[B],
                        fc: F[C],
                        fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(map3(fa, fb, fc)((a: A, b: B, c: C) => f.curried(a)(b)(c)))(fd)


    // derived combinators
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]]
    = as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(fa => fa.map(identity))

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))
      
    def product[G[_]: Applicative]: Applicative[[X] =>> (F[X], G[X])] = new Applicative[[X] =>> (F[X], G[X])] {
      val g: Applicative[G] = summon[Applicative[G]]
      type Ap = [X] =>> (F[X], G[X])
      override def unit[A](a: => A): Ap[A] = (self.unit(a), g.unit(a))

      override def map2[A, B, C](fa: Ap[A], fb: Ap[B])(f: (A, B) => C): Ap[C] = 
        val r1 = self.map2(fa._1, fb._1)(f) 
        val r2 = g.map2(fa._2, fb._2)(f)
        (r1, r2)
    
    }
    
    def compose[G[_]: Applicative]: Applicative[[X] =>> F[G[X]]] = new Applicative[[X] =>> F[G[X]]] {
      val g: Applicative[G] = summon[Applicative[G]]
      type Ap = [X] =>> F[G[X]]
      
      override def unit[A](a: => A): Ap[A] = self.unit(g.unit(a))

      override def map2[A, B, C](fa: Ap[A], fb: Ap[B])(f: (A, B) => C): Ap[C] =
        self.map2(fa, fb)((ga, gb) => g.map2(ga, gb)(f))
         
    }
    
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =  
      ofa.foldLeft(unit(Map.empty[K, V]))((acc, k_fv) => map2(acc, k_fv._2)((acc2, v) => acc2.updated(k_fv._1, v)))
     
 
    extension [A, B](fa: F[A])
      // define in apply and unit
      def map(f: A => B): F[B] =
        apply(unit(f))(fa)  // 函数签名的具体实现
  
      def **(fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  
  type Id = [A] =>> A 
  
  object Applicative:
    given Applicative[LazyList] = new Applicative[LazyList] {
      // The infinite, constant stream 
      override def unit[A](a: => A): LazyList[A] =
        LazyList.continually(a)

      override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
        fa zip fb map f.tupled
    }

    given Applicative[Id] = new Applicative[Id] {
      override def unit[A](a: => A): Id[A] = a
      
      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
        f(fa, fb)
    }
    
    
  
    def test(): Unit = {
      val app = summon[Applicative[LazyList]]
      val r = app.unit(3)
      val r2 = r.take(10).toList 
      println(f"r2 is $r2")
      
      val r3 = app.sequence(List(r))
      val r4 = r3.take(10).toList 
      println(f"r4 is $r4")
    }
        
  trait Monad[F[_]] extends Applicative[F]:
    
    def join[A](ffa: F[F[A]]): F[A] = {
      println(f"join...")
      ffa flatMap (fa => fa )
    }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a =>
        f(a) flatMap g
     
    extension [A, B](fa: F[A])
      def flatMap(f: A => F[B]): F[B] = {
        println("这个方法的优先级很高")
        join(fa map f)
      }

      def doWhile(cond: A => F[Boolean]): F[Unit] =
        for 
          a1 <- fa 
          ok <- cond(a1)
          _ <- if ok then doWhile(cond) else unit(())
        yield 
          ()  
          
      def forever: F[B] =
        println(f"无限循环???")
        lazy val t: F[B] = forever
        fa.flatMap (_ => t)
  
        
  object Monad:
    def eitherMonad[E]: Monad[[A] =>> Either[E, A]] = new Monad[[A] =>> Either[E, A]] {
      type Ei = [AA] =>> Either[E, AA]
      override def unit[A](a: => A): Ei[A] = Right(a)

      override def join[A](ffa: Ei[Ei[A]]): Ei[A] = ffa match
        case Right(r) => r
        case Left(le) => Left(le) 
      
    }
    
    // the OptionT monad transformer composes Option with any other monad.
    case class OptionT[M[_]: Monad, A](value: M[Option[A]]):
      val m: Monad[M] = summon[Monad[M]]
      def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
        OptionT(value flatMap {
          case None => m.unit(None)
          case Some(a) => f(a).value
        })
    
    implicit def stateMonad[S]: Monad[[A] =>> State[S, A]] = new Monad[[A] =>> State[S, A]] {
      type ST = [A] =>> State[S, A]

      override def unit[A](a: => A): ST[A] = State.unit(a)

      override def join[A](ffa: ST[ST[A]]): ST[A] = ???
    }
    
    import Validation._ 
    def validation[E]: Monad[[A] =>> Validation[E, A]] = new Monad[[A] =>> Validation[E, A]] {
      type Va = [AA] =>> Validation[E, AA]

//      override def map2[A, B, C](fa: Va[A], fb: Va[B])(f: (A, B) => C): Va[C] = (fa, fb) match
//        case (Success(a), Success(b)) => Success(f(a, b))
//        case (Success(_), p@ Failure(_, _)) => p.asInstanceOf[Va[C]]
//        case (p@Failure(_, _), Success(_)) => p.asInstanceOf[Va[C]]
//        case (Failure(v1, t1), Failure(v2, t2)) => Failure(v1, v2 +: (t1 ++ t2))
//      
    
      override def unit[A](a: => A): Va[A] = Success(a)
  
      override def join[A](ffa: Va[Va[A]]): Va[A] = ffa match
        case p@ Success(v) => v
        case p => p.asInstanceOf[Va[A]]
    }
  
    def test(): Unit = {
      val m = validation[String] 
      val r = m.unit(3)
      println(f"r is $r")
      
      val r2 = m.unit(m.unit(3))
      println(f"r2 is $r2")
      
      val r3 = r.map(_ * 3)
      println(f"r3 is $r3")
    } 
  
  trait Traverse[F[_]: Applicative] extends Functor[F] with Foldable[F]:
    self =>
    type Const[M , B] = M 
    
    implicit def monoidApplicative[M: Monoid]: Applicative[[A] =>> Const[M, A]] = new Applicative[[A] =>> Const[M, A]] {
      given m: Monoid[M] = summon[Monoid[M]]
      type ST = [A] =>> Const[M, A]
//      override def unit[A](a: => A): ST[A] = m.zero
      override def unit[A](a: => A): M = m.zero

//      override def map2[A, B, C](fa: ST[A], fb: ST[B])(f: (A, B) => C): ST[C] =
      override def map2[A, B, C](fa: M, fb: M)(f: (A, B) => C): M =
        m.op(fa, fb)
    }
    
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(fa map f)  
  
    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(ga => ga)
      
    
    // 通过trverseS中底层通过State来做数据更新
    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[[AA] =>> State[S, AA], A, B](fa)(f)(using Monad.stateMonad) 
      
    def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
      traverseS(ta)((a: A) =>  
          for 
            i <- State.get[Int]
            _ <- State.set(i + 1)
          yield 
            ((a, i))
         ).run(0)._1
        
    override def toList[A](fa: F[A]): List[A] =
      traverseS(fa)((a: A) =>
        for 
          as <- State.get[List[A]]
          _  <- State.set(a :: as)
        yield 
          ()
      ).run(Nil)._2.reverse
      
    // 很显然,(F[B],S)是State中函数返回的数据的结构
    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) =>
        for   
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield 
          b 
      ).run(s)
      
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      (mapAccum(fa, toList(fb)) {
        case (a, Nil) => sys.error(s"zip:In compatible shapes!")
        case (a, b :: bs) => ((a, b), bs)
      })._1
      
    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
      val r = mapAccum(fa, toList(fb)) {
        case (a, Nil) => ((a, None: Option[B]), Nil)
        case (a, b :: bs) => ((a, Some(b)), bs)
      }
    
      r._1

    def zipR[A, B](fa: F[A], fb: F[B]): F[(B, Option[A])] =
      zipL(fb, fa)
      
    def fuse[G[_]: Applicative, H[_]: Applicative, A, B](fa: F[A])(f: A => G[B], g: A => H[B]): (G[F[B]], H[F[B]]) =  
      given g_app: Applicative[G] = summon[Applicative[G]]
      given h_app: Applicative[H] = summon[Applicative[H]]
      
      type T = [X] =>> (G[X], H[X])
      
      given g_h: Applicative[T] = g_app.product[H]
       
      traverse[T, A, B](fa)(a => (f(a), g(a)))
    
    // G本身应具有Applicative的类型 self本身也是具有Traverse的类型
    def compose[GG[_]: Traverse]: Traverse[[X] =>> F[GG[X]]] =  
      type TT = [X] =>> F[GG[X]]
      given ap_gg: Traverse[GG] = summon[Traverse[GG]]
      given ap: Applicative[TT] = summon[Applicative[TT]]
      new Traverse[TT] {
        override def traverse[G[_] : Applicative, A, B](fa: TT[A])(f: A => G[B]): G[TT[B]] =
          self.traverse(fa)(gx => ap_gg.traverse(gx)(a => f(a)))
      }
    
    def composeM[F[_]: Monad, G[_]: Monad: Traverse]: Monad[[X] =>> F[G[X]]] = {
      type TT = [X] =>> F[G[X]]
      given m_f: Monad[F] = summon[Monad[F]]
      given m_g: Monad[G] = summon[Monad[G]]
      given t_g: Traverse[G] = summon[Traverse[G]]
      
      new Monad[TT] {
        override def unit[A](a: => A): TT[A] = m_f.unit(m_g.unit(a))

      }
      

      ???
    }


    // 这个实现还是参考github中的代码逻辑
    // refer 
    // https://github.com/TheDom/functional-programming-in-scala/blob/master/src/main/scala/com/dominikgruber/fpinscala/chapter12/Traverse.scala
    def reverse[A](fa: F[A]): F[A] =  
      mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(as, z)((a, s) => (a, f(s, a)))._2 

    def toList2[A](fa: F[A]): List[A] =
      mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse 
      
    def zipWithIndex2[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1 
      
    // G == [A] =>> Const[M, A] G[X] == M 
    override def foldMap[A, M: Monoid](as: F[A])(f: A => M): M =
      traverse[[A] =>> Const[M, A], A, Nothing](as)(f) // 最后的参数是隐式获取的参数
    
      
    extension [A, B] (fa: F[A])
      def map(f: A => B): F[B] =
        val ap = summon[Applicative[Id]] 
        traverse(fa)(e => ap.unit(f(e)))
         
        
  
  case class WebForm(name: String, birthdate: Date, phoneNumber: String)
  
  object WebForm:
    import Validation._
    def validName(name: String): Validation[String, String] =
      if name != "" then 
        Success(name)
      else
        failure("Name cannnot be empty")
        
    def validBirthdate(birthdate: String): Validation[String, Date] =
      ???
      
    def validPhone(phoneNumber: String): Validation[String, String] =
      if phoneNumber.matches("[0-9]{10}") then 
        Success(phoneNumber)
      else
        failure("Phone number must be 10 digits")
        
    def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
      val m = Monad.validation[String]
      m.map3(validName(name), validBirthdate(birthdate), validPhone(phone))(WebForm(_, _, _))
  
  enum Validation[+E, +A]:
    case Failure(head: E, tail: Vector[E])  
    case Success(a: A) 
    
    def map[B](f: A => B): Validation[E, B] = this match
      case Success(v) => Success(f(v))
      case p => p.asInstanceOf[Validation[E, B]]
  
  object Validation:
    def success[E, A](v: A): Validation[E, A] = Success(v)
    def failure[E, A](v: E): Validation[E, A] = Failure(v, Vector.empty)
  
  @main def start(): Unit = {
    Applicative.test()
    Monad.test()
  }
}
