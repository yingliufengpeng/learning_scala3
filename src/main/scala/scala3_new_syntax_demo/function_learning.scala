package scala3_new_syntax_demo

object function_learning {

  case class Task[A](run: () => A):
    self =>
    def flatMap[B](f: A => Task[B]): Task[B] =
      Task { 
        () => 
          val a = self.run() 
          f(a).run()
      }
  
  
  object Task:
    def block[A](b: => A) = Task(() => b)
    given task_monad: Monad[Task] = new Monad[Task] {
      override def pure[A](v: A): Task[A] = block(v)

      override def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa flatMap f  
    }
  

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  extension [F[_], A](fa: F[A])(using m: Functor[F]) {
    def map[B](f: A => B): F[B] = m.map(fa)(f)
  }

  extension [F[_], A](fa: F[A])(using m: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = m.flatMap(fa)(f)
    def map2[B, C](fb: F[B])(f: (A, B) => C) = m.map2(fa, fb)(f)
    def tuple[B](fb: F[B]): F[(A, B)] = (fa map2 fb)((_, _))

    // derived method
    def <*[B](fb: F[B]): F[A] = (fa tuple fb).map(_._1)
    def *>[B](fb: F[B]): F[B] = (fa tuple fb).map(_._2)

  }

  trait Monad[F[_]] extends Functor[F] {
    def pure[A](v: A): F[A]

    def lift[A, B](f: A => B): A => F[B] = a => pure(f(a))

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(e => pure(f(e)))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      flatMap(fa)(a => map(fb)(b => f(a, b)))
    }
  }
 
  
  extension [F[_], A, B] (f: A => F[B])(using m: Monad[F]) {
    def compose2 [C](g: B => F[C]): A => F[C] =
      a =>
        m.flatMap(f(a))(g(_))

    def |>> [C](g: B => F[C]): A => F[C] = f compose2 g
  }

  object C {

    given m_option: Monad[Option] =  new Monad[Option] {
      override def pure[A](v: A): Option[A] = Some(v)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
    
    implicit def eitherMonad[L]: Monad[[A] =>> Either[L, A]] = new Monad[[A] =>> Either[L, A]] {
      override def pure[A](v: A): Either[L, A] = Right(v)
      override def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa flatMap f
    }
    

    def validate(s: String): Option[String] = {
      if (s.nonEmpty)
        Some(s)
      else
        None
    }


    def test(): Unit = {
      val f1 = m_option.lift((a: Int) => a * 2)
      val f2 = f1 |>> m_option.lift(identity)
      println(f"f2 is ${f2(3)}")
    }

    def test2(): Unit = {
      given m: Monad[Option] = m_option
      val r = validate("Bob") tuple validate("Dole")

      println(f"r is $r")
    }
  }
  
  @main def function_learning_start(): Unit = {
    
    C.test2()
  }
  
  

}
