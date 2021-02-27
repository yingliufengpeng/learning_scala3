package scala3_book

object ch16_dependent_function_types {
  
  trait Entry:
    type Key 
    val key: Key 
  
  def extractKey(e: Entry): e.Key = e.key // a dependent method 
  
  val extractor: (e: Entry) => e.Key = extractKey // a dependent function value
  
  @main def dependent_function_types_strt(): Unit = {
    
    
  }
}
