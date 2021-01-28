package scala3_metaprogrmming
import scala.quoted._
import scala.quoted.runtime._ 
 
object inline_prog {
  
  inline def h(using qctx: Quotes): Expr[Int] = '{3}
  
  // Inline 
  // Inlining is a common compile-time metaprogramming technique, typically used to achieve 
  //  perfomrmance optimizations. We we will see, in Scala3, the concpet of inlining provide 
  //  us with an entrypoint to programming with macros.
  //  
  // 1 It introduces inline as a soft keyword
  // 2 It gurarntees that inlining actually happends instead of being best-effort
  // 3 It introduces operations that are quaranteed to evaluate at compile-time
  //  
  // Inline Constants
  // 
  
 
  inline val pi = 3.1415926
  inline val pie = "\uD83E\uDD67"
  
  def doSomething = 4
  
  @main def inline_start(): Unit = {
    
    inline val pi = 3.1415926
    inline val pie = "🥧"
    
    val pi2 = pi + pi 
    val pie2 = pie + pie 
    
    println(f"pi2 is $pi2 pie2 is $pie2")
    
    // Currently, only constant expresion may appear on the right-hand side of an inline 
    // value defination. Therefore, the following code is invalid, though the compiler
    // knows that the right-hand side is a compile-time constant value.
    val pii = 3.1415
    // inline val pie22 = pii + pii // error 
    // Note that by defining inline val pi, the addition can be computed at compile time. 
    // This resolves the above error and pie22 will receive the literal type 
    
    // Inline Methods 
    // We can also use the modifier inline to define a method that should be inlined at the call-site
    
    inline def logged[T](level: Int, message: => String)(inline op: T): T = {
      println(f"[$level] Computing $message")
      val res = op 
      println(f"[$level] Result of $message: $res")
      res 
    }
    
    // When an inline method like logged is called, it body will be expaned at the call-site at compile time!
    // That is, the call to logged will be replaced by the body of the method. The provied arguments are statically 
    // substituted for the parameters of logged, correspondingly. Therefore, the compiler inlines the following call
    
    logged(4, {4; 5; 6; "kkk"})(doSomething)
    // and rewrites it to 
    val level = 4
    val message = {
      4
      5
      6
      "kkk"
    }
    
    println(f"[$level] Computing $message")
    val res = doSomething 
    println(f"[$level] Result of $message: $res")
    
    res 
    
    // Sematntics of Inline Methods 
    // Our example method logged uses three different kinds of parameters, illustraing
    // that inlining handles those parameters differently:
    // 1 By-value parameters. The compier generates a val binding for by-value parameters. 
    //  This way, the arguemnt expression is evaluated only once before the method is 
    //  reduced.
    //  
    //  This can be seen in the parameter level from the example. In some cases,
    //    when the arguments are pure contant value, the binding is omitted and the 
    //    value is inlined directly
    //
    // 2 By-Name parameters. The compile generate a def binding for by-name parameters.
    //    This way, the argument expression is evaluated every time it is used, but the code 
    //    is shared. This can be seen in the parameter from the example.
    //
    // 3 Inline parameters. Inline parameters do not create bindings and are simply inlined. This
    //    way, their code is duplicated everywhere the are used. This can be seen in the parameter op 
    //    from the example 
    // 
    // The way the different parameters are translated guarantees that inlining a cal will not change 
    //  its semantics. This implies that the initial elaboration(overloading resolution, implicit search,...),
    //  performed while typing the body of the inline method, will not change when inlined.
    //
    // For example, conside the following code:
    //
    class Logger:
      def log(x: Any): Unit = {
        println(x)
      }
    
    class RefinedLogger extends Logger:
      override def log(x: Any): Unit = println(f"Any: $x")
      def log(x: String): Unit = println(f"String: $x")
    
    inline def logged2[T](logger: Logger, x: T): Unit = logger.log(x)
    
    // The separate tpye checking of logger.log(x) will resolve the call to the method 
    // Log.log which tacks an argument of the type Any. Now given the following code:
    //
    //
    logged2(RefinedLogger(), "✔")
    // It expands to 
    val logger3 = RefinedLogger() 
    val x3 = "✔"
    logger3.log(x3)
    // Even though now we know that x is a String, the call logger.log(x) stil resolves
    // to the method Log.log which takes an argument of the type Any
    
    // Inline Parameters
    // One important appliation of inling is to enable constant folding optimation across method boundaries
    // . Inline parameters do not create bindings and their code is duplicated everywhere they are used.
    //
    inline def perimeter(inline radius: Double): Double = 2.0 * pi * radius
    
    val r5 = perimeter(3.0)
    println(f"r5 is $r5")
    
    // In the above example, we expect that if the raduis is statically known then the whole 
    // computation can be performed at compile-time. The followiing call 
    perimeter(5.0)
    // is rewritten to 
    2.0 * 3.1415926 * 5.0
    // Finally, it is constant folded to 
    // 31.4159265359
    
    // Inline parameters should be used only once 
    // We need to be careful when using an inline parameter more than once, Consider the following code:
    inline def printPerimeter(inline raduis: Double): Double = {
      println(f"Perimeter (r = $raduis) = ${perimeter(raduis)}")
      3
    }
    
    // But if a larger expression (possible with side-effects) is passed, we might accidentally duplicate work.
    // printPerimeter(longComputation())  inlined as println(f" Perimeter (r = ${longComputation()}) = ${2.4343434 * longComputation() } ")
    // A useful application of inline parameters is to avoid the creation of closures, incurred by the use of by-name parameter.
    
    def assert1(cond: Boolean, msg: => String) =
      if !cond then 
        throw Exception(msg)
    
    assert1(true, "error1")
    
    // is inlined as 
    val cond = true 
    def msg = "error1"
    if !cond then 
      throw Exception("error1")
      
    // In the above example, we can see that use of a by-name parameter leads to a local definition msg, which allocates
    // a closure before the condition is checked.
    
    // If we use an inline parameter instead, we can guarantee that the condition is checked before any of the code
    // that handleds the exception is reached. In the case of an assertion, this code should never be reached.
    
    inline def assert2(cond: Boolean, inline msg: String) =
      if !cond then 
        throw Exception(msg)
    assert2(true, "error2")
    
    // is inlined as 
    val cond2 = true
    if !cond2 then 
      throw Exception(msg)
    
    // Inline Conditons 
    // If the condition of an if is a known constant (true or false), possibly after inlining and 
    // constant folding, then the conditional is partially evaluated and only one branch will be kept.
    // For example, the fllowing power method constans some if that will potentially unroll the recursion
    // and remove all method calls 
    
    inline def power(x: Double, inline n: Int): Double = { 
      if (n == 0) 1.0
      else if (n % 2 == 1) x * power(x, n - 1)
      else power(x * x, n / 2)
    }
    
    power(2, 2)
    // first inlines as 
    val x1 = 2 
    if (2 == 0) 1.0
    else if ( 2 % 2 == 1) x1 * power(x1, 2 - 1)
    else power(x1 * x1, 2 / 2)
    
    // partially evaluated to 
    val x4 = 2
    power(x4 * x4, 1)
  
    // In contrast, let us imagine we do not know the value of n 
    
    // Driven by the inline annotation on the parameter, the compiler will try to unroll the recursion. But 
    // without any success, since the parameters is not statically known.
    
    // To guarantee that the branchig can indeed to be perfomed at comnpile-time, we can use the inline if variant
    // of if. Annotating with inline will guarantee the conditional can be reduced at compile-time and emits an error
    // if the condition is not a statically known constant.
    
    inline def power2(x: Double, inline n: Int): Double = {
      inline if (n == 0) 1.0
      else inline if (n % 2 == 1) x * power2(x,n - 1)
      else power2(x * x, n / 2)
    }
    
    val r10 = power2(1, 30)
    println((f"r10 is $r10"))
    
    // We will come back to this exampl later and see how we can get more control on how code is generated.
    
    // Inline Method Overriding 
    // Effectively final 
    // Signature preservation  
    //  Secondly, overrides must have the exact same signature at the overriddent method including the inline 
    //    parameters. This ensures thta call semantics are the same for both methods.
    // Retained inline methods 
    
    trait Logger2:
      def log(x: Any): Unit 
    
    class PrintLogger2 extends Logger2:
      inline def log(x: Any): Unit = println(x)
    
    // However, calling the log method directly on PrintLogger will inline the 
    // code while calling it on Logger will not. To aslo admit the latter, the code
    // of log must exist at runtime. We call this a retained inline method
    
    // For any non-retained inline def or val the code can always be fully inlined at all
    // call sites. Heence, those methods will not be needed at runtime and can be erased
    // from the bytecode. However, retained inline methods must be compatible with the case 
    // that they are not inlined. In particular, retained inline methods cannot take any inline 
    // parameters. Furthermore, an inline if (as in the power example) will not work, since 
    // if cannot be constant folded in the retained case. Other examples involve metaprograming 
    // contructs that only have meaning when inline .
    
    // Abstract inine methods
    // It is also possible to create abstract inline definitons 
    
    trait InlineLogger:
      inline def log(inline x: Any): Unit 
    
    class PrintLogger3 extends InlineLogger:
      override inline def log(inline x: Any): Unit = println(x)
    
    // This forces the implemnentation of log to be an inline method and also allows inline parameters.
    // Counterintuitively, the log on the interface InlineLogger cannot be directly called. The method 
    // implementation is not statically known and we thus do not know what to inline. Calling an abstrdct
    // inline methods thus results in an error. The usefuleness of absract inline method becomes apparent 
    // when used in another inline method:
 
    
    inline def logged3(loger: InlineLogger, x: Any) = loger.log(x)

    logged3(PrintLogger3(), 3)
    // inlined as 
    val logger33: PrintLogger3 = PrintLogger3() 
    logger33.log(3)
    
    // After inlining, the call to log is de-virtualized and known to be on PrintLogger. 
    // Therefore also the code of log can be inlined 
    
    
    // Summary of inline methods 
    //  All inline methods are final
    //  Abstract inline methods cna only be implemented by inline methods
    // If an inline method overrides/implements a normal normal method then it must be 
    //  retained and retained methods cannot have inline paramters.
    // Abstract inline methods cannot be called drirectly (except in inline code)
    
    
    // Transparent Inline Methods 
    // Transparent inlines are a simple, yet powerful, extension to inline methods and unlock many
    //  metaprogramming usecases. Calls to transparents allow for an inline piece of code to refine
    //  the return based on the precise type of the inlined expression. In Scala2 paralance, transparents 
    //  capture the essence of whitebox macros.

//    transparent inline def default(inline name: String): 0 | "" | 0.0 =
//      inline if name == "Int" then 0
//      else inline if name == "String" then ""
//      else 
//        0.0
//    
////    default("String")
//    
//    val t1: 0 = default("Int")
//    println(f"t1 is $t1")
    
    // Compiletime Operations 
    // We also provide some operations that evaluate at compile time 
    // Inline Matches 
    // Like inline if , inline matches guarantee that the pattern matching can be statically
    //  reduced at compile time and only one brance is kept
    // In the following example, the scrutinee, x, is an inline parameter that we can pattern match on 
    // at compile time 
    //
    inline def half(x: Any): Any =
      inline x match
        case x: Int => x / 2
        case x: String => x.substring(0, x.length / 2)
    
    half(5)
    // is expanded to 
    val x = 6
    x / 2 
    
    half("hello, world")
    // is expanded to 
    val x2 = "hello world"
    x2.substring(0, x2.length / 2)
    
    // This illustrates that inline matches provides a wy to match on the static type of some expression
    //   Ae we match on the static type of an expression, the following code would fail to compile
    val n: Any = 3 
//    half(n)

    // Notable, The value n is not marked as inline and in consequence at compile time there is not enough
    //  information about the strutinee to decide which branch to take.
    
    // scala.compiletime
    // The package scala.compiletime provides useful metaprogramming abstrations that can be used within inline
    //  methods to provide custom semantics.
    
    // Macros 
    // Inlining is also the core mechainsm used to write macros. Macros provide a wa to control the code generation
    //  and analysis after the call is inlined.
    
//    inline def power33(x: Double, inline n: Int) = ${ powerCode('x , 'n) }
//    
//    def powerCode(x: Expr[Double], n: Expr[Int])(using QuoteContext): Expr[Double] = ???
    
    
    
    
    
    
    
  }

}
