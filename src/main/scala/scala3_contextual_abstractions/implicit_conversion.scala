package scala3_contextual_abstractions
import scala.language.implicitConversions
object implicit_conversion {
  
  // Implicit conversions are defined by given instances of the Scala. Conversion 
  // class. For example, not accounting for possible conversion errors, this code 
  // defines an implicit conversion from String to Int 
  
//  given Conversion[String, Int] with 
//    def apply(s: String): Int = Integer.parseInt(s)
  
  given Conversion[String, Int] = Integer.parseInt(_)
  
  @main def implicit_conversion_start(): Unit = {
    
    def plus1(i: Int) = i + 1
    
    // pass it a String that converts to an Int 
    val r = plus1("4")
    println(f"r is $r")
    
  }

}
