package scala3_book
import scala.util.Sorting

object ch18_context_bounds {
  
  
  def maximum[T: Ordering](xs: List[T]): T = xs.max
  
  
  @main def context_bounds_start(): Unit = {
    
    val r = List(3, 4, 5)
    
    println(f"max of r is ${maximum(r)}")
  }

}
