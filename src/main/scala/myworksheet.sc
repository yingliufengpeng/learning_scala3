import scala.util.Try

println(f"What's function programming?")
enum A {
  case R, G, B
}

val f = (_: Int) + 1
val g = (_: Int) * 2
val h = (_: Int) * 3

val f_gh = f andThen (g andThen h)
val fg_h = (f andThen g) andThen h

f_gh(3) == fg_h(3)

val r = 10

val r2 = f_gh andThen identity
val r3 = identity[Int] andThen f_gh
r2(3) == r3(3)
val r4 = 10
println(r4)

val o_f1: Int => Option[Int] = Some(_)
val o_f2: Int => Option[Int] = Some(_)
val o_f3 = o_f1 andThen (_.map(o_f2).get)

val r5 = Try(4/0)

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

C.test2()















































