package scala3_book

object ch10_extension_method {
  
  
  case class Circle(x: Double, y: Double, radius: Double)
  
  extension (c: Circle)
    def circumference: Double = c.radius * math.Pi * 2 
    def diameter: Double = c.radius * 2 
    
  
  @main def extension_method_start(): Unit = {
    
    val c = Circle(3, 4, 10)
    
    println(f"c is $c")
    
    println(f"c.area is ${c.circumference}")
    
  }

}
