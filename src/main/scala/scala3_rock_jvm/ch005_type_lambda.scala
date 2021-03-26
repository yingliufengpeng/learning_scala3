package scala3_rock_jvm

/*

   kinds = types of types 
   Int, String = value-level kind (level-0) => attach to values 
   List, Option = level-1 kind ("generics")
   Functor, Monad = level-2 kind ("generics of generics")
   
 */

def test(): Unit = {
  val aNumber: Int = 42 
  val aList: List[Int] = List.empty
  class Functor[F[_]]
  val functorOption = Functor[Option]
  
  type MyList = [T] =>> List[T]
  
  val m: MyList[Int] = aList 
  
  type MapWithStringKey = [T] =>> Map[String, T]
  
  val addressBook: MapWithStringKey[String] = Map.empty 
  
  type MapWithStringKey2[T] = Map[String, T]
  
  type SpecialEither = [T, E] =>> Either[E, T]
  
  val specialEither: SpecialEither[Int, String] = Right(3)
  
  // monads 
  trait Monad[F[_]]:
    def pure[A](v: A): F[A]
    extension [A, B](fa: F[A])
      def flatMap(f: A => F[B]): F[B]
  
  
  object Monad:
    def apply[F[_]](using m: Monad[F]) = m 
    
    given eitherMonad[Err]: Monad[[T] =>> Either[Err, T]] with 
      def pure[A](v: A): Either[Err, A] = Right(v)
      extension [A, B](fa: Either[Err, A])
        override def flatMap(f: A => Either[Err, B]): Either[Err, B] = fa flatMap f 
  
    def test(): Unit = {
      val r = apply[[X] =>> Either[String, X]]
      println(s"r is $r")
    }
  
  Monad.test()
} 

@main def ch005_type_lambda_start(): Unit = {
  test()
}

