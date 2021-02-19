package scala3_other_changed_features

import scala.annotation.targetName

object rules_for_operators {
  
  trait MultiSet[T]:
    type ThisType = MultiSet[T]
    infix def union(other: ThisType): ThisType
    def difference(other: ThisType): ThisType
    @targetName("intersection")
    def *(other: ThisType): ThisType
  
  end MultiSet
  
  infix type OR[X, Y] = Map[X, Y]
  type AND[X, Y] = Map[X, Y]
  
  def fun1[T](x: MultiSet[T], y: MultiSet[T]): Unit = {
    val r = x * y 
    val r2 = x.difference(y) 
    val r3 = x difference y
  }
  
  // List[?] =:= List[? >: Nothing <: Any]
    
  type S = [X] =>> List[X]
  
  type T = S[?]
  
  
  // def unapply[A](x: T)(using x: B): U
  // Fixed-Arity Extractors
  // The type U conforms to one of the following matches:
  //    Boolean  match
  //    Product  match
  // Or U conforms to the type R
  // type R  = {
  //    def isEmpty: Boolean
  //    def get: S
  // and S conforms to one of the following matches:
  //  single match
  //  name-based match
  // The  former form of unapply has higher procedence, and single match has 
  //  higher precedence over name-based match
  // U = true
  // U = Some[T]
  // U <: R and U <: { def isEmpty:  false  }
  
  
  
  // def unapplySeq[A](x: T)(using x: B): U 
  // The type U conforms to one of the following matches:
  //  sequence match
  //  product-sequence match
  // Or U conforms to the type R:
  //  type R = {
  //      def isEmpty: Boolean
  //      def get: S
  //   }
  // and S conforms to one of the two matches above
  // The former form of unapplySeq has higher priority, and sequence match has higher
  // precedence over product-sequence match
  // A usage of a variadic extractor is irrefutable if one of the following confiditons
  // holds:
  //    the extractor is used directly as a sequence match or product-sequence match
  //    U = Some[T]
  //    U <: R and U <: { def isEmpty: false }
  //  Boolan  Match
  //    U =:= Boolean 
  //    Pattern-matching on exactly 0 pattern
  //
  
  
  object Even:
    def unapply(s: String): Boolean = s.size % 2 == 0

  class FirstChars(s: String) extends Product:
    def _1 = s.charAt(0)
    def _2 = s.charAt(1)
    def _3 = s.charAt(2)
  
    // Not used by pattern matching: Product is only used as a marker trait.
    def canEqual(that: Any): Boolean = ???
    def productArity: Int = ???
    def productElement(n: Int): Any = ???

  object FirstChars:
    def unapply(s: String): FirstChars = new FirstChars(s)
  
  
  class Nat(val x: Int):
    def get: Int = x
    def isEmpty = x < 0 
  
  object Nat:
    def unapply(x: Int): Nat = Nat(x)
  
  
  class Foo(val name: String, val children: Int*)
  object Foo:
    def unapplySeq(f: Foo): Option[(String, Seq[Int])] =
      Some((f.name, f.children))
  
  def foo(f: Foo) = f match {
    case Foo(name, x, y, ns*) => println(f"second ")
    case Foo(name, ns*) => println(f"first ...")
  }
  
  object ProdEmpty:
    def _1: Int = 1
    def _2: Int = 2 
    def isEmpty = false
    def get = this 
    def unapply(x: String): this.type = this 
  
  
  object CharList:
    def unapplySeq(s: String): Option[Seq[Char]] = Some(s.toList)
  
  
  def fun2(): Unit = {
    "even" match
      case s @Even() => println(f"$s has an event number of characters")
      case s => println(f"$s has an old number of characters")
    
    
    "Hi!" match
      case FirstChars(c1, c2, c3) =>
        println(f"first: $c1, second: $c2 third: $c3")
    
    5 match
      case Nat(n) => println(f"$n is a natural number")
      case _ => ()
    
    
    "12" match
      case ProdEmpty(c1, c2) => println(f"$c1, $c2")
      case _ => ()
    
    
    "example" match
      case CharList(c1, c2, c3, ns:_*) => println(f"ok")
      case _ => println("Expected *exactly* 7 characters!")
    
    val r10 = Foo("3", 3, 45, 5)
    foo(r10)
    
  }
  
  @main def ruels_for_operators_start(): Unit = {
    
    val r: OR[String, Int] = Map.empty
    val r2: String AND Int = Map.empty
    
    val xs = List(3, 4, 5)
    
    val r3 = if xs match
          case Nil => false
          case _ => true 
        then "nonempty"
        else "empaty"
     
    println(f"r3 is $r3")
    
    val r4 = xs match
      case List(1, 2 ,tail:_*) => println(tail)
      case List(1, _:_*) => println(1)
      case _ => println(0)
    
    println(f"r4 is $r4")
    
    val first :: second :: rest = xs 
    println(f"first is  $first $second")
    
    fun2()
  }
 
  
  

}
