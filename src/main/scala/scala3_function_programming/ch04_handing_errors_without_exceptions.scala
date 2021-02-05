package scala3_function_programming

object ch04_handing_errors_without_exceptions {
  
  // Next time you try writing some functional code that uses Option, see if 
  // you can recognize the patterns these functions enanpsulate before you 
  // resort to pattern matthing
  
  enum MyOption[+A]:
    case Some(v: A)
    case None
    
      
    def map[B](f: A => B): MyOption[B] = this match
      case Some(v) => Some(f(v))
      case None => None 
    
    
    def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match
      case None => None
      case Some(v) => f(v)
  
    
    def getOrElse[B >: A](default: => B): B = this match
      case Some(v) => v
      case None => default
  
    
    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match
      case None => ob
      case p => p 
  
    
    def filter(f: A => Boolean): MyOption[A] = this match
      case p@ Some(v@x) if f(v) => p
      case _ => None 
   
    
    def lift[B, C](f: B => C): MyOption[B] => MyOption[C] = _ map f 
  
  
  object MyOption:
    
    def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
//      a.foldRight(MyOption(List[A]()))((v, acc) =>  map2(v, acc)((v2, acc2) => v2 :: acc2) )
      a.foldRight(MyOption(List.empty[A]))((v, acc) =>  map2(v, acc)((v2, acc2) => v2 :: acc2) )

    
    def apply[A](v: A): MyOption[A] = Some(v)
    
    
    def empty[A]: MyOption[A] = None 
    
    
    def Try[A](a: => A): MyOption[A] =
      try 
        Some(a)
      catch
        case e: Exception => None 
        
    
    def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    
    
    def map3[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))
      
    
    def map4[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
      for 
        aa <- a 
        bb <- b
      yield 
        f(aa, bb)
    
    
    def flatten[A](o: MyOption[MyOption[A]]): MyOption[A] = o match
      case None => None 
      case Some(v) => v
 
    
    def parseInts(a: List[String]): MyOption[List[Int]] =
        MyOption.sequence(a map (i => MyOption.Try(i.toInt)))

    def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
      a.foldRight(Some(List.empty[B]))((v, acc) => map2(f(v), acc)((v2, acc2) => v2 :: acc2))

    def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] =
      traverse(a)(e => e)


  case class Employee(name: String, department: String)
  
  
  def lookupByName(name: String): MyOption[Employee] = MyOption.Some(Employee("kk", "jj"))
  
  def insuranceRateQuote(age: Int, numberOfSpeediingTickets: Int): Double = age * numberOfSpeediingTickets
  
  def parseInsuraneRateQuote(
                            age: String,
                            numberOfSpeedTickets: String
                            ): MyOption[Double] =
    val optAge = MyOption.Try {age.toInt}
    val optTicktes = MyOption.Try{ numberOfSpeedTickets.toInt}
    MyOption.map2(optAge, optTicktes)(insuranceRateQuote)
    
  
  def variance(xs: Seq[Double]): MyOption[Double] = xs match
    case Nil => MyOption.empty
    case p =>
      val n = p.length  
      val r = p.indices.flatMap(i => (i + 1 until n).map(j => math.pow(p(i) - p(j), 2))).sum 
      MyOption(r / n * n )

     

  @main def handing_erros_start(): Unit = {
    
    val r = List(List(3, 4), List(4, 5))
    val r2 = r.flatten
    println(f"r2 is $r2")
    
    val joeDepartment: MyOption[String] =
      lookupByName("Joe").map(_.department)
      
    println(f"joeDepartment is $joeDepartment")
    
    val r3 = variance((1 to 100).toSeq.map(_.toDouble))
    println(f"r3 is $r3")
    
    val r5 = List(MyOption(4), MyOption(5), MyOption(6))
    val r6 = MyOption.sequence(r5)
    println(f"r6 is $r6")


    val r7 = MyOption.parseInts(List("4", "5", "6", "z"))
    println(f"r7 is $r7")

    val r8 = MyOption.traverse(List("4", "5", "67"))(e => MyOption.Try {e.toInt})
    println(f"r8 is $r8")


    val r9 = MyOption.sequence2(List(MyOption("4"), MyOption("d"), MyOption.None))
    println(f"r9 is $r9")
    
    
  }

}
