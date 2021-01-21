import scala.collection.mutable.ListBuffer

object Intersection_types {
  
  class MyStack[A]:
    private var elements: List[A] = Nil 
    
    def push(x: A): Unit = { elements = x :: elements }
  
    def peek: A = elements.head
    
    def pop(): A = 
      val currentTop = peek 
      elements = elements.tail 
      currentTop
  
  
  

  trait Animal:
    def speak(): Unit
  
  def double(ints: List[Int]): List[Int] =
    val buffer = ListBuffer.empty[Int]
    for (i <- ints) {
      buffer += i * 2
      buffer += i 
    }
    buffer.toList
  
  @main def main2() = {
    val r = double(List(3, 4, 5))
    println(r)
    
    val stack = MyStack[Int]
    stack.push(1)
    stack.push(2)
    println(stack.pop())
    println(stack.pop())
    
  }

}
