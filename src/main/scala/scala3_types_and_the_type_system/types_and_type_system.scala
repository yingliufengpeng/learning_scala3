package scala3_types_and_the_type_system

object types_and_type_system {
  
  class Stack[A]:
    private var elements: List[A] = Nil 
    def push(x: A): Unit = 
      elements = x :: elements
    
    def peek: A = elements.head
    def pop(): A =
      val currentOtp = peek 
      elements = elements.tail
      currentOtp
  
  
  def fun1(): Unit = {
    val stack = Stack[Int]
    stack.push(1)
    stack.push(2)
    println(stack.pop())
    println(stack.pop())
  }
  
  trait Resettable:
    def reset(): Unit 
  
  trait Growable[A]:
    def add(a: A): Unit 
  
  
  def f(x: Resettable & Growable[String]): Unit = {
    x.reset() 
    x.add("first")
  }
  
  trait Both[A] extends Resettable, Growable[String]
  
  def f2(x: Both[String]): Unit = ???

  // There is an important difference between two altenatives of defing f:
  // While both allow f to be called with instance of Both, only the former allows 
  // passing instances that are subtypes of Resettable and Growable[String]
  // but not of Both[String]
  // Note & is commutative: A & B is the same type as B & A 
  
  
  def fun2(): Unit = {
    
  }
  
  
  case class Username(name: String)
  case class Password(hash: String)
  
  def fun3(): Unit = {
    val name = Username("Eve")
    val password = Password("123")
    
    val r = if (true) name else password
    val r2: Username | Password = if (true) then name else password
    
    println(f"type(r) is ${r.getClass}")
    println(f"type(r2) is ${r2.getClass}")
    
    
  }
  
  enum Color:
    case Red, Green, Blue 
  
  enum HSI(val hsi: Int):
    case H extends HSI(1)
    case S extends HSI(2)
    case I extends HSI(3)
    case Mix(mix: Int) extends HSI(mix)
  
  def fun4(): Unit = {
    println(Color.Green)
    println(HSI.H)
  }
  
  enum Planet(mass: Double, radius: Double):
    private final val G = 6.67300E-11
    def surfaceGravity = G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double) = otherMass * surfaceGravity
    
    case Mercury extends Planet(3.303e+13, 2.4397e6)
    case Venus   extends Planet(4.869e+24, 6.0518e6)
    case Earth   extends Planet(5.976e+24, 6.37814e6)
  
  def fun5(): Unit =  
    import Planet._ 
    val earthWeight = 33
    val mass = earthWeight / Earth.surfaceGravity 
    for 
      p <- values
    do 
      println(f"Your weight on $p is ${p.surfaceWeight(mass)}" )
    
  
  // The enum concept is general enough to also support algebraic data types(ADTs)
  // and theit generalized version(GADTs). Here's an example that show how an Option 
  // type can be repersented as an ADT 
  
  enum MyOption[+T]:
    case Some(x: T)
    case None 
    
    def isDefined: Boolean = this match
      case None => false
      case Some(_) => true 
  
  object MyOption:
    def apply[T >: Null](x: T): MyOption[T] =
      if (x == null) None else Some(x)
  
  
  def fun6(): Unit = {
    val r = MyOption.Some(4)
    val r2 = MyOption.None 
    
    println(f"r is $r")
    println(f"r2 is $r2")
    
    val r3 = MyOption(4)
    val r4 = MyOption(null)
    println(f"r3 is $r3")
    println(f"r4 is $r4")
    
  }
  
  // Recursive enumerations 
  enum Nat:
    case Zero
    case Succ(n: Nat)
  
    def Number: Int = this match
      case Zero => 0
      case Succ(nat) => 1 + nat.Number
  
  def fun7(): Unit = {
    import Nat._ 
    val r = Succ(Succ(Succ(Zero)))
    println(f"r's number is ${r.Number}")
  }
  
  enum MyList[+A]:
    case Nil
    case Cons(head: A, tail: MyList[A])
  
    def length: Int = this match
      case Nil => 0
      case Cons(h, tail) => 1 + tail.length

  def fun8(): Unit = {
    import MyList._ 
    val r = Cons(1, Cons(2, Cons(3, Nil)))
    println(f"r's length is ${r.length}")
  }
  
  enum Box[T](contents: T):
    case IntBox(n: Int) extends Box[Int](n)
    case BoolBox(b: Boolean) extends Box(b)
 
  
  object Box:
    def extract[T](box: Box[T]): T = box match
      case IntBox(n) => n
      case BoolBox(b) => b
  
  def fun9(): Unit = {
    import Box._ 
    val r = IntBox(3)
    val r2 = extract(r)
    println(f"r2 is $r2")
  }
 
  @main def types_and_type_system_start(): Unit = {
    fun1()
    fun3()
    fun4()
    fun5()
    fun6()
    fun7()
    fun8()
    fun9()
  }

}
