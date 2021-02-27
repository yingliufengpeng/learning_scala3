package scala3_book

object ch10_extension_method {
  
  
  case class Circle(x: Double, y: Double, radius: Double)
  
  extension (c: Circle)
    def circumference: Double = c.radius * math.Pi * 2 
    def diameter: Double = c.radius * 2 
  
  case class A[T](v: T)
  
  extension [T] (v: A[T])
    def map[B](f: T => B): A[B] =
      A(f(v.v))
  
    
  
  @main def extension_method_start(): Unit = {
    
    val c = Circle(3, 4, 10)
    
    println(f"c is $c")
    
    println(f"c.area is ${c.circumference}")
    
    val r = A(4)
    val r2 = r.map(_ * 2)
    println(f"r2 is $r2")
    
    
  }

}
