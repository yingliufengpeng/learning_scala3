package scala3_function_programming

object ch04_handing_errors_with_either {
  
  enum MyEither[+E, +A]:
    case Left(value: E) 
    case Right(value: A)
  
    
    def map[B](f: A => B): MyEither[E, B] = this match
      case Left(v) => Left(v)
      case Right(v) => Right(f(v))
    
    
    def map2[EE >: E,B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
      for 
        a <- this 
        b <- b
      yield 
        f(a, b)
  
    
    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match
      case Left(v) => Left(v)
      case Right(v) => f(v)
  
    
    def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match
      case Left(v) => b 
      case Right(v) => this 
  
  
  object MyEither:
    
    
    def apply[E, A](v: A): MyEither[E, A] = Right(v)
    
    
    def error[E, A](v: E): MyEither[E, A] = Left(v) 
    
    
    def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
      if xs.isEmpty then 
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)
        
    
    def safeDiv(x: Int, y: Int): MyEither[Exception, Int] = Try(x / y)
      
    
    def Try[A](a: => A): MyEither[Exception, A] =
      try 
        Right(a)
      catch
        case e: Exception => Left(e)
        
    
    def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
      as.foldRight( MyEither[E, List[B]](List.empty ))((a, acc) => f(a).map2(acc)((a2, acc2) => a2 :: acc2 ))
      
  
    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      traverse(es)(e => e)
  
  
    case class Name(value: String)
    case class Age(value: Int)
    case class Person(name: Name, age: Age)
    
    
    def mkName(name: String): MyEither[String, Name] =
      if name == "" then 
        Left("Name is Empty")
      else
        Right(Name(name))
        
    
    def mkAge(age: Int): MyEither[String, Age] =
      if age < 0 then 
        Left("Age is out of range")
      else
        Right(Age(age))
        
    
    def mkPerson(name: String, age: Int): MyEither[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_, _))
  
  
  @main def handing_errorss_with_either_start(): Unit = {
    
    val r = MyEither[String, Int](34)
    println(f"r is $r")
    
    val r2 = MyEither.error[String,Int]("kk")
    println(f"r2 is $r2")
    
    val r3 = r.map(e => e * 2)
    println(f"r3 is $r3")
    
    val r4 = r2.map(e => e * 3)
    println(f"r4 is $r4")
    
    val r5 = r.map2(r3)((a, b) => a + b)
    println(f"r5 is $r5")
    
    val r6 = List(r, r3)
    println(f"r6 is $r6")
    val r66 = MyEither.sequence(List(r, r3))
    println(f"r66 is $r66")
    
     
  }

}
