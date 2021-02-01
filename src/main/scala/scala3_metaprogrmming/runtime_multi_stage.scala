package scala3_metaprogrmming
import scala.quoted._ 


object runtime_multi_stage {
    
    // What if we could synthesize code at run-time and offer one extra stage to the programmer? Then 
    //  we can have a value of type Expr[T] at run-time that we can essensitally treat as a typed-syntax 
    //  tree that we can either show as a string(pretty-print) or compile and run. If the number of quotes 
    //  exceeds the number of splices by more than one (effetively handling at run-time values of type Expr[Expr[T]]), 
    //  then we talk about Multi-Stage Programming.
    
    
    // 1 A top-level splice must appear in an inline method (turing that method into macros)
    // 2 The splice must call a previously compiled mehthod passing quoted argumetns,
    //      constant arguments or inline arguments
    // 3 Splice inside splices(but no intervening quotes) are not allowd.
    // Now we can make a future-stage function of [Array[Int] => Int] 
    // Using staging.run { ... } we can eval 
    
    // With the scope of staging.run we can also invoke on an 
    //  expression to get a source-like representation of the expression.
    
    
    
    // make available the necessary compiler for runtime code generation 
//    given staging.Compiler = staging.Compiler.make(getClass.getName)
    
    @main def runtime_multi_stage_start(): Unit = {
        val r = 3
        println(f"r is ${r.getClass.getName}")
    }
}
