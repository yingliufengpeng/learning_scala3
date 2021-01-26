package scala3_oher_changed_features

object automatic_eta_expansion {
   
  def m(x: Boolean, y: String)(z: Int): List[Int] = List(3)
  def next(): Int = 3
  val f1 = m 
  val f2 = m(true, "abc")
  val f3 = () => next()
   
  @main def automatic_eta_expansion_start(): Unit = {
    
    println(f1(true, "abc")(3))
    println(f2(3))
    println(f3())
    
    lazy val m = f3() 
    println(f"m is $m")
    
  }

}
