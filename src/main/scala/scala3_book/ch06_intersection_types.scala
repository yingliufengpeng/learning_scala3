package scala3_book

object ch06_intersection_types {
  
  trait Resetable:
    def reset(): Unit 
  
  trait Growable[A]:
    def add(a: A): Unit 
  
  def f(x: Resetable & Growable[String]): Unit =
    x.reset() 
    x.add("first")
  
  
  class SubReset extends Resetable:
    override def reset(): Unit = println("  reset")

  class SubGrow extends Growable[String]:
    override def add(a: String): Unit = println(f"  adding a ${a}")
  
  class S_Reset_Grow extends Resetable with Growable[String]:
    override def reset(): Unit = println("reset")
    override def add(a: String): Unit = println(f" adding a ${a}")

  // The members of an intersection type A & B are all the memers of A and all the 
  // members of B. Therefore, as shown, Resetable & Growable[String] has member 
  // member method reset and add.
  
  
    
  @main def intersection_types_start(): Unit = {
    val s = S_Reset_Grow() 
    
    f(s)
    
  }
}
