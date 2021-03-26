package scala3_rock_jvm

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
 

def test_ch006(): Unit = {
  
  // if-expression 
  // one-liner
  
  val aCondtion = if (2 > 3) "bigger" else "smaller"
  
  // compact 
  val aCondition2 =
    if (2 <3)
      "bigger"
    else 
      "samller"
      
  // scala3 one-liner
  val aCondition5 = if 2 > 3 then "Bigger" else "smaller"
  
  val aCondition6 =
    if 2 > 3 then 
      "Bigger"
    else
      "Smaller"
    
  println(s"aCondition6 is $aCondition6")
  
  
  val r = 
    for 
      n <- List(1, 2, 3)
              c <- List("a", "b", "c") 
    yield 
           s"$c$n"
  
  
  println(s"r is $r")
  
  def computeMeaningOfLife(year: Int): Int = 
    println(s"Computing")
    
    34
  
       443
        55
//    44
  
  val r2 = computeMeaningOfLife(33)
  println(s"r2 is $r2")
  
  given myOrder: Ordering[Int] with 
    def compare(x: Int, y: Int): Int = x - y 
  
  
  val aFuture = Future {
     
    3
  }
  
  
  
  val aProcessedList = List(1, 2, 3) map (e => Future{ e * 2})
  
  
  
  val x = 30
  if (x < 0) {
    println(1)
    println(2)
  println(3)  
  }
  
  def solution(input: String): Boolean =
    if input.size % 2 == 1 then 
      false
    else 
      input.splitAt(input.size / 2).zipped.forall((e1, e2) => e1 == e2)
      
  val r3 = solution("[]{{[]}}")
  println(s"r3 is $r3")
    
   
}
@main def ch006_indentation_start(): Unit = {
  test_ch006()
   
}
