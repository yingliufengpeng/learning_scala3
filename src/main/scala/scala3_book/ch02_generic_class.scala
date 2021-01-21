package scala3_book

object ch02_generic_class {

  class MyStack[A]:
    private var elements: List[A] = Nil

    def push(x: A): Unit = { elements = x :: elements }

    def peek: A = elements.head

    def pop(): A =
      val currentTop = peek
      elements = elements.tail
      currentTop
  
  
  @main def start() = {

    val stack = MyStack[Int]
    stack.push(1)
    stack.push(2)
    println(stack.pop())  // prints 2
    println(stack.pop())  // prints 1
  }

}
