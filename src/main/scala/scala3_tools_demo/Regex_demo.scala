package scala3_tools_demo

object Regex_demo {
  
  trait Foo
  
  @main def regex_demo_start(): Unit = {
    val reg = " ".r 
    val r  = reg.pattern.matcher("dddd ddd")
    println(f"r is $r")
    
    val r2: List[_ <: Foo] = List(new Foo{}, new Foo{})
  }

}
