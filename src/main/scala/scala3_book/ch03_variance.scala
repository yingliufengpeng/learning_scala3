package scala3_book

object ch03_variance {
  
  trait Item:
    def productNumber: String 
  
  trait Buyable extends Item:
    def price: Int 
  
  trait Book extends Buyable:
    def isBn: String


  trait Pipeline[T]:
    def process(t: T): T

  trait Producer[+T]: 
    def make: T 
  
  trait Consumer[-T]:
    def take(t: T): Unit


  /**
   * 
   * We connot pass a Pipeline[Book] to the method oneOf because in its implementation 
   * , we call p1 and p2 with a vallue of type buyable. A Pipeline[Book] expects 
   * a Book, which can potentially cause a runtime error. 
   * 
   * We cannot pass a Pipelinie[Item] because calling process on it only promise to return 
   * an Item; however, we are supposed to return a Buyable 
   * 
   * In face, type Pipeline needs to be invariant since it uses its type parameter T 
   * both as an argument and as a return type. For the same reason, some types in the scala
   * colletion library -- like Array or Set -- are alse invariant
   * 
   * @param p1
   * @param p2
   * @param b
   * @return
   */
  def oneOf(
           p1: Pipeline[Buyable],
           p2: Pipeline[Buyable],
           b: Buyable
           ): Buyable =
    val b1 = p1.process(b)
    val b2 = p2.process(b)
    
    if b1.price < b1.price then b1 else b2


  /**
   * Marking it as convariant means that we can pass(or return) a Producer[Book] where 
   * a Producer[Buyable] is expected. And in fact,this is sound: The type off Producer[Buyable]
   * .make only promises to return Buyable. As a cller of make, we will be happy to also accept 
   * a Book, which is a subtype of Buyable -- this is, it is at least a Buyable.
   * 
   * 
   */
  def makeTwo(p: Producer[Buyable]): Int =
    p.make.price + p.make.price
    
  
  trait Function[-A, +B]:
    def apply(a: A): B 
  
  // 协变与逆变的写法
  val f: Function[Buyable, Buyable] = b => b 
  
  // g 声明的类型[Buyable, Item] 
  val g: Function[Buyable, Item] = f 
  
  // h 声明的类型[Book, Buyable]  f 为实际的类型
  val h: Function[Book, Item] = f 
  
   
  @main def variance_start: Unit = {
    
    println(s"h is ${h} ")
    println(f"china is ${3}")
  }

}
