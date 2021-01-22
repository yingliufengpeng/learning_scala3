package scala3_book

object ch17_polymorphic_function_types {
  
  // A polymorphic method 
  def foo[A](xs: List[A]): List[A] = xs.reverse
  
  // A polymorphic function value: 
  val bar: [A] => List[A] => List[A] = // a polymorphic function type 
    [A] => (xs: List[A]) => foo(xs)
  

  enum Expr[A]:
    case Var(name: String)
    case Apply[A, B](fun: Expr[B => A], arg: Expr[B]) extends Expr[A]

  import Expr._

  // We would like to provide a way for users to map function over all immediate subexpression of 
  // a given Expr. This requires the given funcion to be polymorphic, since each subexpression may have 
  // different type. Here is how to implement his using polymorphic function types.
  def mapSubexpressions[A](e: Expr[A])(f: [B] => Expr[B] => Expr[B]): Expr[A] =
    e match
      case Apply(fun, arg) => Apply(f(fun),f(arg))
      case Var(n) => Var(n)
  
  // Polymorphic function types are not to be confused with type lambdas. While the former describes the type 
  // of a polymorphic value, the latter is an actual function value at type level.
  
  // A good way of understanding the difference is to notice that type lambdas are applied in types, whereas polymorphic 
  // are applied in terms: One would call the function bar above by passing it a type argument bar[Int] within a method 
  // body. On the other hadn, given a type lambda such as type F = [A] =>> List[A], one call F within a type expression, 
  // as in type Bar = F[Int] 
  
  @main def ploymorphic_function_types_start(): Unit = {
    
    val r = bar(List(3, 4))
    println(f"r is $r")
  
    val e0 = Apply(Var("f"), Var("a"))
    val e1 = mapSubexpressions(e0)(
      [B] => (se: Expr[B]) => Apply[B, B](Var("wrap"), se) 
    )
    
    val e2 = mapSubexpressions(e0)([B] => (se: Expr[B]) => Apply(Var[B => B]("wrap"), se))
    
    println(f"e0 is $e0")
    println(f"e1 is $e1")
  }

}
