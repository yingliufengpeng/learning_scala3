package scala3_book

object ch01_type_inference {
  val businessName = "WangPeng"
  
  def squareOf(x: Int) = x * x 
  
  def fac(n: Int): Int = if (n == 0) 1 else n * fac(n - 1)
  
  case class MyPair[A, B](x: A, y: B) 
  
  val p = MyPair(1, "Scala")
  
  def id[T](x: T) = x 
  
  val q = id(1)
  
  
  Seq(1, 3, 4).map(x => x * 2) 
  
  
  var obj = null 
  
  // We can't then go on and make this reassignment 
  obj = null
  
  
  
  
  
  
  
}
