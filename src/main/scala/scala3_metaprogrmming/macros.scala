package scala3_metaprogrmming

import scala.quoted._ 

import scala.collection.immutable.{TreeSet, HashSet}

object macros {

//  inline def foo(inline x: Int,  y: Int)(using Quotes): Int = ${impl(x,'{y})}
  inline def foo(inline x: Int,  y: Int)(using Quotes): Expr[Int] = Expr(y)

  def impl(x: Int, y: Expr[Int]) = y

  inline def assert(inline expr: Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
    if !$expr then
      throw AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
    }

  def showExpr(expr: Expr[Boolean])(using Quotes): Expr[String] =
    '{ "<some source code>" } // Better implementation later in this document

  
  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = 
    '{ (x: T) => ${ f('x) } }
    
  // The resulting value of Type will be subject to PCP. Indeed, the definition of to above uses T in the next 
  //   stage, there is a quote but no splice between the parameter binding of T and its usage. But the code can
  //   be rewritten by adding a binding of a Type[T] tag.
  def to2[T, R](f: Expr[T] => Expr[R])(using Type[T], Type[R], Quotes): Expr[T => R] = 
    '{ (x: T) => ${ f('x) } }
  
  // 这里面的Expr[ T => R ]到底是什么东西呢?  $f =:= T =>  $expr_x =:= x
  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }
    
  // To avoid clutter, the Scala imolementation tries to convert any type reference to a type T in subsequent 
  //  paases to a tpye-splice, by rewriting T to summon[Type[T]].Underlying. For instance, the user-level definition
  //  of to
  def to3[T, R](f: Expr[T] => Expr[R])(using t: Type[T], r: Type[R])(using Quotes): Expr[T => R] = 
    '{ (x: T) => ${ f('x) } }
    
  enum Exp:
    case Num(n: Int)
    case Plus(e1: Exp, e2: Exp)
    case Var(x: String)
    case Let(x: String, e: Exp, in: Exp)
  
  // Here's a compiler that maps an expressin given in the interperted language to quoted Scala code of type 
  //  Expr[Int]. The compiler takes an enviorment that maps variable names to Scala Exprs.
  // looks luspicious, n is declared as an Int, yet it is converted to an Expr[Int] with Expr(). Should't n quoted?
  //  In fact this would not work since replacing n by 'n in the cluase would not be phase correct.
  object Exp:
    def compile(e: Exp, env: Map[String, Expr[Int]])(using Quotes): Expr[Int] = e match
      case Num(n) => Expr(n)
      case Plus(e1, e2) => '{ ${ compile(e1, env) } + ${ compile(e2, env)} }
      case Var(x) => env(x)
      case Let(x, e, body) => 
        '{ val y = ${ compile(e, env) }; ${ compile(body, env + ( x -> 'y)) } }

  // Scala3 comes with given instances of ToExpr for sereral types including Boolean, String and all primitive number
  //  types. For emample, Int values can be conerted to Expr[Int] values by wrapping the value in a Literal tree node
  //  . This makes use of the underlying tree representation in the compiler for efficiency. But the ToExpr instances 
  //    are nevertheless not magic in the sence that they could all be fefined in a user program without knowning anything
  //    about the representation of Expr trees. For instane, here is a possible instance of ToExpr[Boolean].
  //
  
//  given ToExpr[Boolean] with
//    def toExpr(b: Boolean)(using Quotes) = if b then '{ true } else '{ false }
  
  // Once we can lift bits, we can work our way up. For instance, here is a possible implementation of ToExpr[Int]
  //    that does not use the underlying tree machinery.
  
//  given ToExpr[Int] with
//    def toExpr(n: Int) = ???
  
  def showExpr2[T](expr: Expr[T])(using Quotes): Expr[String] =
    val code: String = expr.show 
    Expr(code)
  
//  inline def summon_2[T](using Quotes) = '[ List[ ${ summon[Type[T]] }   ] ]
//  inline def summon_3[T](using Quotes) = Type.of[ List[ ${ summon[Type[T]] } ] ]
  object Macros:
    inline def assert(inline expr: Boolean): Unit =
      ${ assertImpl2('expr) }

    def assertImpl2(expr: Expr[Boolean])(using Quotes) =
      val failMsg: Expr[String] = Expr("failed assertion: " + expr.show)
      '{ if !($expr) then throw new AssertionError($failMsg) }
  
  
  inline def power(x: Double, inline n: Int) = ${ powerCode('x, 'n) }
  
  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
    n.value match
      case Some(m) => powerCode(x, m)
      case None => '{ Math.pow($x, $n.toDouble)  }

  def powerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
    if n == 0 then '{ 1.0 }
    else if n == 1 then x
    else if n % 2 == 0 then '{ val y = $x * $x; ${ powerCode('y, n / 2) } }
    else '{ $x * ${ powerCode(x, n - 1) } }
    
  def fun1(): Unit = {
    
  }
  
  object Macros2:
    def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])(using Type[T], Quotes): Expr[Unit] = 
      '{
        var i: Int = 0 
        while i < ($arr).length do 
          val element: T = ($arr)(i)
          ${f('element)}
          i += 1
      } 
      
    def sum(arr: Expr[Array[Int]])(using Quotes): Expr[Int] = 
      '{
        var sum = 0 
        ${ map(arr, x => '{ sum += $x }) }
        sum
      }
      
    inline def sum_m(arr: Array[Int]): Int = ${sum('arr)} 
  
  def fun2(): Unit = {
    
  }
  
  
  inline def setFor2[T]: Set[T] = ${setForExpr2[T]}
  
  def setForExpr2[T: Type](using Quotes): Expr[Set[T]] =
    Expr.summon[Ordering[T]] match
      case Some(ord) => '{ TreeSet[T]()($ord)  }
      case _ => '{ HashSet[T]()  }
  
  
  transparent inline def defaultOf(inline str: String) =
    ${ defaultOfImpl('str) }
    
  def defaultOfImpl(strExpr: Expr[String])(using Quotes): Expr[Any] =
    strExpr.valueOrError match
      case "int" => '{1}
      case "string" => '{"a"}
      
  object Macros4:
    inline def sum(inline args: Int*): Int = ${sumExpr('args)}
    def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] =
      argsExpr match
        case Varargs(args @ Exprs(argValues)) =>
          // args is of type Seq[Expr[Int]]
          // argValues is of type Seq[Int]
          Expr(argValues.sum) // precompute result of sum 

        case Varargs(argExprs) => // argExprs is of type Seq[Expr[Int]]
          val staticSum: Int = argExprs.map(_.value.getOrElse(0)).sum 
          val dynamicSum: Seq[Expr[Int]] = argExprs.filter(_.value.isEmpty)
          dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })

        case _ => '{ $argsExpr.sum }
  
  object Macros5:
    def sum(args: Int*): Int = args.sum 
    inline def optime(inline arg: Int): Int = ${optimizeExpr('arg)}
    def optimizeExpr(body: Expr[Int])(using Quotes): Expr[Int] =
      body match
        // match a call to sum without any argruments
        case '{ sum() } => '{0} // Expr(0)
        // Match a call to sum with an argument $n of type Int. 
        // n will be the expr[Int] representing the argument.
        case '{ sum($n) } => n
        // Match a call to sum and extracts all its args in ann `Expr[Seq[Int]]`
        case '{ sum(${Varargs(args)}: _*) } => sumExpr(args)
        case body => body
    
    def sumExpr(args1: Seq[Expr[Int]])(using Quotes): Expr[Int] =
      def flatSumargs(arg: Expr[Int]): Seq[Expr[Int]] = arg match
        case '{ sum(${Varargs(subArgs)}: _*) } => subArgs.flatMap(flatSumargs)
        case arg => Seq(arg)
  
      val args2 = args1.flatMap(flatSumargs)
      val staticSum: Int = args2.map(_.value.getOrElse(0)).sum
      val dynamicSum: Seq[Expr[Int]] = args2.filter(_.value.isEmpty)
      dynamicSum.foldLeft(Expr(staticSum))((acc, arg) => '{ $acc + $arg })
  
  
  def f(expr: Expr[Any])(using Quotes) = expr match
    case '{ $x: t } => 
      // If the pattern match succeeds, then there is some type `t` such that 
      // - `x` is bound to a variable of type `Expr[t]`
      // -  `t` is bound to a new type `t` and a `given` instance `Type[t]` is provided for it 
      //  That is, we have `x: Expr[t]` and `given Type[t]`, for some (unknow) type `t`. 
      //    
  
  extension (inline  sc: StringContext)
    inline def showMe(inline args: Any*): String = ${ showMeExpr('sc,'args) }
  
  trait Show[-T]:
    def show(t: T): String 
  
  given Show[Boolean] with
    def show(b: Boolean) = "boolean!"

  def showMeExpr(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[String] =
    import quotes.reflect.report
    argsExpr match
      case Varargs(argExprs) =>  // 变参的参数, 
        val argShowedExprs = argExprs.map { // 从一种变参转换为另一种变参的情况
          case '{ $arg: tp } =>
          Expr.summon[Show[tp]] match
            case Some(showExpr) =>
              '{ $showExpr.show($arg) }
            case None =>
              report.error(s"could not find implicit for ${Type.show[Show[tp]]}", arg); '{???}
        }
        val newArgsExpr = Varargs(argShowedExprs)
        '{ $sc.s($newArgsExpr: _*) }
      case _ =>
        // `new StringContext(...).showMeExpr(args: _*)` not an explicit `showMeExpr"..."`
        report.error(s"Args must be explicit", argsExpr)
        '{???}
  
  
  def f2(using Quotes): Expr[Int] = '{ 
    val x: Int = 4
    x * x 
  }
   
  def fun3(): Unit = {
     
    
  } 
  // To match such a term we need to match the defintion and the rest of the code, but we need to explicitly 
  //   state that the rest of the code may refer to this definition
  
  // case '{ val y: Int = $x; $body(y): Int } => 
  // Here $x will match any closed expression while $body(y) will match an expression that is closed under y. Then 
  //  the subexpression of tyep Expr[Int] is bound to body as an Expr[Int => Int]. The extra argument represents the 
  //  references to y. Usually this expression is used in combination with Expr.betaReduce to replace the extra argument.
  //  We can also close over serval binding usng $b(a1, a2, a3, ..., an). To match an actual application we can use
  //  braces on the funtion part ${b}(a1, a2, ..., an). 
  inline def eval(inline e: Int): Int = ${ evalExpr('e) }
  
  def evalExpr(e: Expr[Int])(using Quotes): Expr[Int] = e match
    case '{ val y: Int = $x ; $body(y): Int } =>  // betaReduce 这样的规约的模型,我很喜欢,,
      // body: Expr[Int => Int] where the argument represents references to y
      evalExpr(Expr.betaReduce('{ $body(${evalExpr(x)}) })) // evalExpr( Expr.betaReduce( '{ $body( ${ evalExpr(x) }) } ) )
    case '{ ($x: Int) * ($y: Int) } =>
      (x.value, y.value) match
        case (Some(a), Some(b)) => Expr(a * b)
        case _ => e
    case _ => e 
   
  @main def macros_satrt(): Unit = {
    
    fun1()
    fun2()
    fun3()
    
    val r = 120 
    println(f"r is $r")
    
//    val f1: Expr[Int => String] = to((x: Expr[Int]) => '{ $x.toString})
//    val f2: Expr[Int] => Expr[String] = from( '{ (x: Int) => x.toSTring } )
    
    // One limitation of from is that it does not β-reduce when a lambda is called immediately, as 
    // evidenced in the code { ((x: Int) => x.toString)(2) }. In some cases we want to remove the lambda
    //  from the code, for this we provide the method Expr.betaReduce that turns a tree describiing a function
    //  into a function mapping trees to trees.
    // Expr.betaReduce(_): Expr[(T1, ..., Tn) => R] => ( (Expr[T1], ..., Expr[Tn]) => Expr[R])
    
    
    import Exp._ 
    val exp = Plus(Plus(Num(2), Var("x")), Num(4))
    val letExp = Let("x", Num(3), exp)
    
    given x: Int = 0 
    val r2 = summon[Int]
    println(f"r is $r2")
    
//    val ctx = summon[Quotes]
    
     
//    val r4 = compile(letExp, Map()) 
//    
//    println(f"r4 is $r4")
    
    
    // Members of refiable types are handled by just reifying the containing type together with the member name. 
    //  But what to do for references to tpe parameters or local type definitions that are not defined in the 
    //  current stage? Here, we cannot construt the Type[T] tree directly, so we need to get it from a recursive 
    //  implicit search.
    // For instance, to implement
    given List[Int] = List.empty[Int]
    val rr0 = summon[Int]
    
    println(f"rr0 is $rr0")
     
    val rr1 = summon[List[Int]]
    println(f"rr1 is $rr1") 
    
    // Type[T] is abstract type tree 
//    val rr3 = summon[Type[List[Int]]]
    
     
 
    
  }

}
