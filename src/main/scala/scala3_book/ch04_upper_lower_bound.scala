package scala3_book

object ch04_upper_lower_bound {
  abstract class Animal:
    def name: String 
  
  abstract class Pet extends Animal 
  
  class Cat extends Pet:
    override def name: String = "Cat"
  
  class Dog extends Pet:
    override def name: String = "Dog"
  
  class Lion extends Animal:
    override def name: String = "Lion"
  
  
  class PetContainer[P <: Pet](p: P):
    def pet: P = p 
  
  @main def u_l_start: Unit = { 
    println("kk")
    
    val dogContainer = PetContainer(Dog())
    val catContainer = PetContainer(Cat())
    
    val africanSwallowList = ListNode(AfricanSwallow(), Nil)
    val enropeanSwallow = EuropeanSwallow() 
    
    val birdList: Node[Bird] = africanSwallowList
    
    val birdList2 = birdList.prepend(enropeanSwallow)
    
    println(f"birdLlist is ${birdList2}")
    
  }
  
  
  
  trait Node[+B]:
    def prepend[U >: B](elem: U): Node[U]
  
  case class ListNode[+B](h: B, t: Node[B]) extends Node[B]:
    override def prepend[U >: B](elem: U): ListNode[U] = ListNode(elem, this)
    
    def head: B = h 
  
    def tail: Node[B] = t
  
  case object Nil extends Node[Nothing]:
    override def prepend[U >: Nothing](elem: U): Node[U] = this 
    
  
  trait Bird 
  trait SubBird extends Bird 
  
  case class AfricanSwallow() extends Bird 
  case class EuropeanSwallow() extends SubBird 
  
  
  
  
  

}
