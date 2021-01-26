package scala3_other_new_feathures

object matchable_trait {

  def f[T](x: T) = x.asInstanceOf[Matchable] match
    case a: Array[Int] => a(0) = 0
  
  @main def matchable_trati_start(): Unit = {
    val imm: IArray[Int] = IArray(3, 4)
    f(imm)
    println(f"imm is ${imm.toArray.toList}")
    
    val r = Object() 
    println(f"r is $r")
    val r2: AnyRef = r 
    println(f"r2 is $r2")
    
  }
}
