package scala3_book
import scala.language.strictEquality

object ch15_match_types {
  
  type Elem[X] = X match 
    case String => Char 
    case Array[t] => t 
    case Iterable[t] => t 
  
  // [String]           =:=  Char
  // Elem[Array[Int]]   =:=  Int
  // Elem[List[Float]]  =:=  Float
  // Elem[Nil.type]     =:=  Nothing
  
  type T = Elem[String]
  opaque type M = Elem[String]
  
  type T1 = String 
  opaque type T2 = String  
  
  // In general, a match type is of the form 
  // S match { P1 => T1 .. Pn => Tn } and P1, ..., Pn are type patterns, Tpye variables in patterns
  // start witha lower case letter, as usual. 
  
  // Match types can form part of recursive type definitions
  type LeafElem[X] = X match 
    case String => Char 
    case Array[t] => LeafElem[t]
    case Iterator[t] => LeafElem[t]
    case AnyVal => X 
  
  // Recursive match type definitions are alse be given an upper bound, like this 
  type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match 
    case EmptyTuple => Ys 
    case x *: xs => x *: Concat[xs, Ys]
  // In this definition, every instance of Concat[A, B], whether reducible or not, is known to be 
  // a subtype of Tuple. This is necessary  to make the recursive invocation x *: Concat[xs, Ys] type
  // check, since *: demands a Tuple as its right operand 
  
  // Dependent Typing
  // Match types can be used to define dependently typed methods. For instance, here is the value
  // counterpart to the LeafElem type defined above (note the use of the match type as the return 
  // type):
  
  def leafElem[X](x: X): LeafElem[X] = x match
    case x: String => x.charAt(0)
    case x: Array[t] => leafElem(x(0))
    case x: Iterator[t] => leafElem(x.next())
    case x: AnyVal => x
  
  // This special mode of typing for match expression is only used when the following conditions are 
  // met: 
  // 1 The match expression pattern do not have guards
  // 2 The match expression strutinee's type is a subtype of the match tpye scrutinee's type 
  // 3 The match expression and the match type have the same number of cases 
  // 4 The match expression patterns are all Typed Patterns, and these types are =:= to their 
  //    correspoindiing type patterns in the match type 
  
  // S match { P1 => T1, ..., Pn => Tn }
  // is Match(S, C1, ..., Cn) <: B 
  // where each case Ci is of the form [Xs] =>> P => T 
  // Here, [Xs] is a tpe paramter clause of the variables bound in pattern Pi. If there are no bound 
  // type variables in a case, the type parameter clause is omitted and only the function type P => T
  // is kept. So each case is either a unary function type or a type lambda over a unary funtion type.
  // B is the declared uppper bound of the match type, or Any if no such bound is given. We will leave
  // it out in places where it does not matter for the discussion. The scrutinee, bound and patern types
  // must all be first order types.
  
  
  // Match Type Reduction 
  // Match type reduction follows the semantics of match expression, that is, a match type of the form 
  // S match { P1 => T1, ..., Pn => Tn } reduces to Ti if and only if s: S match { _: P1 => T1, ... 
  // _: Pn => Tn} evaluates to a values of types Ti for all s: S 
  
  
  // The compiler implements the following reduction algorithm:
  // If the scrutinee type S is an empty set of values (such as Nothing or String & Int), do not deduce
  // Sequentially consider each pattern Pi 
  //    If S <:Pi reduce to Pi 
  //    Otherwise, try constructing a proof that S and Pi are disjoint, or, in other words, that 
  //      no value s of type S is also of type Pi 
  //    If such proof is found, proceed to the next case (Pi + 1), otherwrise, do not reduce.
  
  // Disjointness proofs rely on the following properties of Scala types:
  // 1 Single inheritance of classes
  // 2 Final classes cannot be extended 
  // Constant types with distinct values are nonintersecting 
  // Singleton paths to distinct values are nonintersecting, such as object definitions or singleton
  //    enum cases.
  
  
  // Type parameter in patterns are minimally iinstantiated when computing S <: Pi. An instantiaton Is
  // is minimal for Xs if all type variables in Xs that appear convariantly and nonvariantly in Is are
  // as small as possible and all type variables in Xs that appear contravariantly in Is are as large as 
  /// possible. Here "small" and "large" are understood with respect to <: .
  
  // For simplicity, we have omitted constraint handing so far. The full formulation of subtyping tests
  // describes them as a function from a constraint and a pair of types to either success and a new 
  // constraint or failure. In the context fo reduction, the subtyping test S <: [Xs := Is] P is understood
  // to leave the bounds of all variables in the input constraint unchanged, i,e. existing variables 
  // in the constraint cannot be instantiated by matching the scrutinee against the patterns.
  
  
  // Variance Laws for Match Type 
  // Within a match type Match(S, Cs) <: B, all occurences of type variables count as covariant. By 
  // the nature of the cases Ci this means that occurrences in pattern position are contravariant 
  // (since patterns are represented as function type arguments )
  
  type L[X] = X match 
    case Int => L[X]
  
  def g[X]: L[X] = ???
  
   
  @main def match_types_start(): Unit = {
     
    val r: T = 'k'  
    
    val r2: M = 'k'
    
    val r3: T1 = "k"
    val r4: T2 = "k"
    
    println(r3 == r4)
    
    println(r == r2)
    
  }

}
