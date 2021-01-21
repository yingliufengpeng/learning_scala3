package scala3_book

object ch05_polymorhpic_methods {
  
  def listOfDulicates[A](x: A, length: Int): List[A] =
    if (length < 1) then Nil else x :: listOfDulicates(x, length - 1)
  
  
  @main def polymorphic_start(): Unit = {
    val m = listOfDulicates(3, 10)
    println(f"m is $m")
  }
  

}
