package parser_combinator

object demo {
  
  
  enum Region:
    def outer: Region | Null 
    
    case InParas(char: Char, outer: Region)
    case InBraces(outer: Region)
  
 
     
     
  
    
  
  @main def demo_start(): Unit = {
    
    
  }

}
