package scala3_scala_for_python_developer
import reflect.Selectable.reflectiveSelectable

object for_python_developer {
  type Par = [X] =>> Future[X]
  
  trait Future[+A]:
    def apply(k: A => Unit): Unit
  
  trait Source {
    def readBytes(
                   numBytes: Int,
                   callback: Either[Throwable, Array[Byte]] => Unit): Unit
  }
  
  def async[A](run: (A => Unit) => Unit): Par[A] = new Future {
    def apply(k: A => Unit) = run(k)
  }

  def nonblockingRead(source: Source, numBytes: Int): Par[Either[Throwable,Array[Byte]]] =  
    val r = async { (cb: Either[Throwable,Array[Byte]] => Unit) =>
      source.readBytes(numBytes, cb)
    }
    r{ a => 
      println("kkk")
    }
    r
  

  class Su:
    def show = 300
  
  extension (a: Su)
    def show = 4
  
  def test_su(): Unit = {
    val r = Su() 
    println(f"r is ${r.show}")
  }

  trait Consumer[-T]:
    def consume(v: T): Unit

  trait Producer[+T]:
    def producer: T
  
  type Record = {
    def show: Int 
  }
  
  trait A: 
    def show: Int 
  class B:
    def show: Int = 3
  
  
  def show_record(r: Record): Unit = {
    r.show
  }
  
  def fun_record(f: Int => Int) =
    new {
      def show(x: Int): Int = f(x)
    }.asInstanceOf[Record]
  
  def test2(): Unit = {
    val r = {
      for 
        i <- 1 to 100
      do { 
        i
//        println(f"i is $i")
      
      }
    }
    
    println(f"r is $r")

  }
  

  type Cons = [T] =>> T => Unit
  type Pro = [T] =>> () => T
  type P[+T] = Int => (Int => T)
  
  def test1(): Unit = {
    test2()
    
    val r: Cons[Int] = println(_)
    val r2: Pro[Int] = () => 3
    
    r(33)
    r2()
  }
  
  def test4(): Unit = {
    val r = for 
        e <- 1 to 100 
        if e % 2 == 0  
        if e % 3 == 0
      yield 
        e 
    println(f"r is $r")
  }
  
  def test5(): Unit = {
    val r = for 
      i <- 1 to 10 if i % 2 == 0
      j <- 1 to 5
    yield 
      (i, j)
    println(f"r is $r")
  }
  
  
  @main def python_developer_start(): Unit = {
    test2()

    test5()

    test_su()
    test1()
    test4()
    
    val movies = Map(
      "Toy Story" -> 8.3,
      "Forrest Gump" -> 8.8,
      "Cloud Atlas" -> 7.4
    )
    
    val x = Set(1, 2, 3)
    
    val x2 = (11, "Eleven")
    
    class Person(var name: String): 
      def speak = println(f"Hello, my name is $name")
    
    val p = Person("John")
    
    def add(a: Int, b: Int): Int = a + b 
    
    def walkThenRun() = {
      println(f"Walk")
      println(f"run")
    }
    
    val x5 = 1
    if x5 == 1 then println(x5)
    
    if x5 == 1 then { 
      println(f"x5 is 1, as you can see:")
      println(x5)
    }
    
    
    if x5 < 0 then 
      println("negative")
    else if x5 == 0 then 
      println("zero")
    else
      println("positive")
    
    def min(a: Int, b: Int): Int =
      if a < b then a else b 
    
    var i = 1
    while i < 3 do 
      println(i)
      i += 1
    
    // prefered 
    for i <- 0 until 3 do println(i)
    
    // also avaliable 
    for (i <- 0 until 3) println(i)
    
    // mutliline syntax
    for 
      i <- 0 until 3
    do 
      println(i)
    
    val ints = List(3, 4, 5)
    for i <- ints do println(i)
    
    for 
      i <- ints
    do 
      println(i)
      
    for 
      i <- 1 to 2
      j <- 4 to 5
      k <- 1 until 10 by 3 
    do 
      println(f"i = $i, j = $j, k = $k")
    
    
    for 
      i <- 1 to 10
      if i % 2 == 0
      if i < 5
    do 
      println(i)
    
    for 
      i <- 1 to 10
      if i % 2 == 0 && i < 5
    do 
      println(i)
      
    
    val x6 = 
      for 
        i <- 1 to 3 
      yield 
        i * 10
        
    
    val x7 = 
      try 
        1 / 0
      catch 
        case e: Exception => 3    
    
    println(f"x7 is $x7")
    
    object ** :
      def unapply(ab: (Int, Int)): Option[Int] = Some(4)
    
    (4, 54) match
      case **(a) => println(f"a is $a")
      case _ => println("error")
    
  }
}
