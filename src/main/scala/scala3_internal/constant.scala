package scala3_internal
import dotty.tools.dotc.core.Constants._ 
object constant {

  @main def constant_start(): Unit = {
     
    val r = Constant(3)
    println(f"r is $r")
  }
}
