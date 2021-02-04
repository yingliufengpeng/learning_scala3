package scala3_function_programming
import scala.annotation

object ch02_getting_start_with_programming {
  
  def sum(list: Int*): Int =  
    def go(index: Int = 0, acc: Int = 0): Int =
      if index >= list.length then
        acc
      else
        go(index + 1, acc + list(index))  

    go()
  

  def factorial(n: BigInt): BigInt =
    
    @annotation.tailrec
    def go(n: BigInt, acc: BigInt): BigInt =
      if (n <= 0) then  
        acc
      else
        go(n - 1, n * acc)
    
    go(n, 1)
  
  def fib(m: Int): BigInt =
    
    @annotation.tailrec
    def go(index: Int = 1, prev: BigInt=0, last: BigInt = 0): BigInt =
      if m == index then
        prev + last
      else if index == 1 then 
        go(2, 0, 1)
      else if index == 2 then 
        go(3, 0, 1)  
      else
        go(index + 1, last, prev + last)

    go()
  
  def fib2(n: Int): Int =
    
    if n <= 1 then 
      0
    else if n == 2 || n == 3 then 
      1  
    else
      fib2(n - 1) + fib2(n - 2)
        
  
  def formatResult(name: String, n: Int, f: Int => Int) =
    s"The $name of $n is ${f(n)}"
    
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec 
    def loop(n: Int): Int =
      if n >= as.length then 
        -1
      else if (p(as(n))) 
        n
      else
        loop(n + 1)
        
    loop(0)
  
  def isSorted[A](as: Array[A], orderd: (A, A) => Boolean): Boolean = 
    def go(index: Int = 0, acc: Boolean = true): Boolean =
      if index >= as.length - 1 || !acc then 
        acc
      else 
        go(index + 1, orderd(as(index), as(index + 1)))

    go()
  
  def partiall[A, B, C](a: A, f: (A, B) => C): B => C =  
    b => 
      f(a, b)

  def partiall2[A, B, C](a: A, f: (A, B) => C): B => C =
    b =>
      f.curried(a)(b)
      
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =  
    a =>
      b => 
        f(a,b)
   
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) =>
      f(a)(b)
      
  // f andThen g is the same as g compose f
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a =>
      f(g(a))
  

  @main def ch02_getting_start_with_programming_start(): Unit = {
    val r = factorial(5)
    println(s"r is $r")
    val r2 = fib(50)
    println(s"r2 is $r2")
    val r3 = fib2(1)
    println(s"r3 is $r3")
    
    val r4 = formatResult("factorial", 7,  fib2)
    println(s"r3 is $r4")
    
    val r5 = findFirst(Array(1, 2, 3), t => t == 3)
    println(s"r5 is $r5")
    
    val r6 = isSorted(Array(1, 2, 4, 6), (left, right) => left < right)
    println(s"r6 is $r6")
    
    val r7 = sum(1,2,3,4)
    println(s"r7 is $r7")
    
  }

}
