package scala3_tools_demo

object demo2 {

  trait A
  trait B 
  trait MA:
    def get: A 
  
  trait MB:
    def get: B
  
  
  class M extends MA with MB:
    override def get: A & B = new B with A {
    }
  
  object M1:
    class A:
      def size: Int = ???
    class B:
      def size: Int = ???
  
    val x: A & B = ???
    val y = x.size 
  
  
  def test(): Unit = {
    trait A(x: String):
      println(f"x is $x")
    
    val r = new A("冰冰狗") {}
    def f(x: 1): 1 = x 
    val x: 1 = 1 
    val y: List[1 | String] = List(1, "kk", 1, x, f(1))
    
    val r2: Null | String = "33"
    val r3: String = "44"
    
    
  }
  
  @main def demo2_start(): Unit = {
    val x: 1 | "2" = if true then 1 else "2"
    
    x match
      case 1 => println(1)
      case p => println(p)
    
    test()
  }
}
