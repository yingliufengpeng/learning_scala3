package scala3_function_programming

object ch05_strictness_and_laziness {
  
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = 
    if cond then onTrue() else onFalse()

  def if3[A](cond: Boolean, onTrue:  => A, onFalse:  => A): A =
    if cond then onTrue else onFalse 
    
  def maybeTwice(b: Boolean, i: => Int): Int = if b then i + i else 0 
  
  def maybeTwice2(b: Boolean, i: => Int): Int =
    lazy val r = i 
    if b then 
      r + r
    else
      0


  @main def stritness_and_laziness_start(): Unit = {
    val r = maybeTwice(true, {println("hi"); 1 + 2})
    val r2 = maybeTwice2(true, {println("hi"); 1 + 2})
    
    println(f"r is $r")
    println(f"r2 is $r2")
  }

}
