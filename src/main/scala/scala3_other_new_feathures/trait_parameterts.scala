package scala3_other_new_feathures

object trait_parameterts {
  
  trait Greeting(val name: String):
    def msg = f"How are you , $name"
  
  class C extends Greeting("Bob"):
    println(f"msg is $msg")
  
//  class D extends C, Greeting("Bill")
  
  trait FormalGreeting extends Greeting:
    override def msg: String = f"How do you do, $name"
  
  class E extends Greeting("Bob"), FormalGreeting 
  class F extends FormalGreeting, Greeting("Bob")  
  class G(name: String) extends Greeting(name), FormalGreeting
  
  @main def trait_parameters_start: Unit = {
    val e = E() 
    val f = F() 
    val g = G("kk")
    println(f"e is $e, f is $f g is $g")
  }

}
