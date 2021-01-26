package scala3_oher_changed_features
import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits

object numeric_literals {
  
  class A 
  given FromDigits[A] with
    def fromDigits(digits: String): A = A() 
  
  
  @main def numeric_literals_start(): Unit = {
    
    val x: Long = -10_000_000_000
    val y: BigInt = 0X123_abc_789_def_345_678_901
    val z: BigDecimal = 110_222_799_799.99
    
    y match
      case x => println(f"x is $x")
    
   
    
  }

}
