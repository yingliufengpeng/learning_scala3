package scala3_other_new_feathures
import scala.annotation.targetName


object targetname_annotation {
  
  object VecOps:
    type Vec = [T] =>> List[T]
    extension [T](xs: Vec[T])
      @targetName("append2") // ++= could b invoked from Java like this: VecOps.append(vec1, vec2)
      def ++= (ys: Vec[T]): Vec[T] = xs ++ ys    
  
  
  class A2:
    def f(): Int = 1
  
  class B2 extends A2:
    @targetName("f2") def g(): Int = 2
  
  // The relevant overiding rules can be sumarized follows:
  //  Two members can override each other if their names and signatures are the same, and they either have the same erased
  //      name or the same types
  //  Tf two members override, then both thir erased names and their types must be the same.
  // As usual, any overriding relationship in the generated code must also be present in the original code. So the following 
  // example would also be in error.
  
  @targetName("f_string")
  def f(x: => String): Int = x.length 
  def f(x: => Int): Int = x + 1
  
  @main def targetname_annotation_start(): Unit = {
    import VecOps._ 
    val m = List(3, 4)
    
    val n = m ++= List(4, 5)
    f(3)
    f("3")
    
    val x = 10
    if x < 0 then 
      "negation"
    else if x == 0 then 
      "zero"
    else
      "positive"
      
    if x < 0 then -x else x 
    
    var y = 100
    while 
      y > 0 
    do
      println(f"y is $y")
      y -= 1
    
    for x <- m if x > 0
    yield x * x 
    
    for 
      x <- m
      y <- m 
    do 
      println(x + y)
      
    
    try 
      f(x)
    catch
      case ex: Exception => ()
    
    
  }

}
