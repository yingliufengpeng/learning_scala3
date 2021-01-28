package scala3_metaprogrmming

object scala_compile_time_operations {
  
  @main def scala_comple_time_operations_start(): Unit = {
    // It is possible to emmit error message when inlining code.
    
    
    inline def doSomething(inline mode: Boolean): Int | Double = {
      if  mode then 3
      else if  !mode then 4.0
      else
        throw Exception(f"Mode must be a known value but got: $mode")
     
    }
    
    def r_booeal(n: Int) = if n % 2 == 0 then true else false
    val r = doSomething(4 == 5)
    val r2 = doSomething(5 == 5)
    println(f"r is $r")
    println(f"r2 is $r2")
//    
    val bool: Boolean = r_booeal(3)
    
    val r4 = doSomething(bool)
    println(f"r4 is $r4")
    
    // If error is called outside an inline method the error will be emitted when 
    //  compiling that call. If the error is written inside an inline method 
    // the error will be emitted only if after inlining the call in not removed 
    // as part of a dead branch. In the previous example we used the value of mode 
    // is known we woudl only keep one of the first two branches.
    
    // If we want to include part the souce code of the grguments in the error
    // message we can use the code string interpolator
    
    // Summoning 
    // There are two ways to summon values in inline methods, the first is with
    //  a using parameters and the second is with one of summonInline, summonAll
    // or summonFrom. using will summon the value at call site before inlining 
    // as if the method was not inline. On the other hand, summonInine will 
    // sumon after inlining if the call is not elimintaed form a dea brance. 
    // Summon all provides a way to summon multiple values at the same from 
    // a tupel type. summonFrom provides a way to try several implicit serarchs.
    //
    
    // Values 
    // constValue, constValueOpt and constValueTuple
    // s Comming sonn 
    
    // Testing 
    // testing.typeChecks and testing.typeCheckErros
    
    // the first is with a using parameter and the scond is  with one of
    // summonInline, summonAll or summonFrom.  using will summon the valuee 
    // at call site before inlining as if the method was not inline.
    // On the other hand, summonInline will summon after inlining if the 
    // call is not eliminated form a dead branch. Summon all provides a way\
    // to summon multiple values at at the same time from a tuple type.
    // summonFrom provide a way to try several implicit searchs.
    
 
  }

}
