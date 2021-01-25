package scala3_types_and_the_type_system

object variance {
  
  trait Item:
    def productNumer: String 
  
  trait Buyable extends Item:
    def price: Int 
  
  trait Book extends Buyable:
    def isbn: String 
  
  
  // an example of an invariant type 
  // In fact, type Pipeline needs to be invariant since it uses its' type paramter T 
  // both as an argument and as a return type. For the same reason, some types in the 
  // Scala collection library -- like Array or Set -- are also invariant.
  trait Pipeline[T]:
    def process(t: T): T 
  
  // an example of an covariant type 
  // Marking it as covariant means that we can pass(or return) a Producer[Book] where 
  // a Producer[Buyable] is expected. And in fact, this is sound: The type of Producer[Buyable]
  // .make only promises to return a Buyable. As a caller of make, we will be happy to
  // also accept a Book, which is a subtype of Buyable -- that is, it is at least a Buyable.
  trait Producer[+T]:
    def make: T 
  
  // an example of an contravariant type 
  // Making it as contravariant means that we can pass(or return) a Producer[Item]
  // where a Producer[Buyable] is expected. That is, we have the subtyping relationship
  // Producer[Book] <: Producer[Buyable]. Remember, for type Consumer, it was the other
  // way arount, and we had Consumer[Item] <: Consumber[Buyable]
  trait Consumer[-T]:
    def take(t: T): Unit 
  
  
  def oneOf(
           p1: Pipeline[Buyable],
           p2: Pipeline[Buyable],
           b: Buyable
           ): Buyable =
    val b1 = p1.process(b) 
    val b2 = p2.process(b)
    if b1.price < b2.price then b1 else b2 
  
  def makeTwo(p: Producer[Buyable]): Int =
    p.make.price + p.make.price
  
  @main def variance_start(): Unit = {
    
    val f: Function[Buyable, Buyable] = b => b 
    val g: Function[Buyable, Item] = f 
    val h: Function[Book, Buyable] = f 
    val m: Function[Book, Item] = g 
    println(f"f is $f")
    
  }
}
