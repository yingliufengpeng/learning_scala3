package scala3_tools_demo

import scala.quoted.* 




@main def metaprogramming_demo_start(): Unit = {
  val r = Macros.test2()
  println(s"r is $r")
  
}
