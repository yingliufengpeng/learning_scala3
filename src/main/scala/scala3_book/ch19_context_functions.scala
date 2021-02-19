package scala3_book

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext

object ch19_context_functions {
  
  type Executable[T] = ExecutionContext ?=> T 
  type Executable2 = [T] =>> ExecutionContext ?=> T 
  
  given ec: ExecutionContext = ExecutionContext.global
   
  def f(x: Int): ExecutionContext ?=> Int = x 
  
  def f2(x: Int): Executable[Int] = x
  
  def f3[T](x: T): Executable[T] = x 
  
  def f4[T](x: T): Executable2[T] = x 
  
  
  
  // Conversely, if the expected type of an expression E is a context function type 
  // (T_1, T_2, ... , T_n) ?=> U and E is not already an context function literal, E 
  // is converted to a context function literal by rewriting it to 
  // (x_1: T1, ,,,, x_n: Tn) ?=> E where the names x_1, ... x_n are arbitray. This expansion 
  // is performed before the expression E is typechecked, which means that x_1, ... x_n are 
  // available as given in E.
  
  // Like their types, context function literal are written using ?=> as the arrow between paramters
  // and resultts. They differ from normal function literals in that their types are context function
  // types
  
  def g[T](arg: Executable2[T]): Int = 3
  
  // Context functino types have considerable expressive power. For instance, here is how they can 
  // support the "builder pattern", where the aim it to construct tables like this:
  
  
  class Table:
    val rows = ArrayBuffer.empty[Row]
    def add(r: Row): Unit = rows += r

    override def toString: String = rows.mkString("Table(", ", ", ")")
  
  
  class Row:
    val cells = ArrayBuffer.empty[Cell]
    def add(c: Cell): Unit = cells += c

    override def toString: String = cells.mkString("Row(", ", ", ")")
  
  case class Cell(elem: String)
  
  
  def table(init: Table ?=> Unit) =
    given t: Table = Table() 
    init 
    t
  
  def row(init: Row ?=> Unit)(using t: Table) =
    given r: Row = Row() 
    init 
    t.add(r)
  
  
  def cell(str: String)(using r: Row) = {
    r.add(Cell(str))
     
  }
  
  // Example: Postconditions 
  // As a larger example, here is a way to definde contructs for checking arbitrary postconditions using 
  // an extension method ensuring so that the checked result can be refered to simply by result 
  // The example combines opaque type alias, context function types, and extension methods to 
  // provide a zero-overhead abstraction 
  
  
  // Explanations: We use a context function type WrappedResult[T] ?=> Boolean as the type of the 
  // condition of ensuring. An argument to ensuring such as (result == 6) will therefore have a given 
  // of type WrappedResult[T] in scope to pass along to the result. WrappedResult is a fresh type, to 
  // make sure that we do not get unwanted givens in scope (this is good practice in all cases where context
  // parameters are involved). Since WrappedResult is an opaque type alias, its values need not be boxed
  //, and since ensuring is added as an extension method, its argument does not need boxing either. 
  // Hence, the implementation of ensuring is as about as efficient as the best possible code one 
  // could write by hand.
  object PostConditions:
    // 不透明的引用,防止外界引用真正的类新,以便导致未知定义的出现
    opaque type WrappedResult[T] = T 
    
    
    def result[T](using r: WrappedResult[T]): T = r 
    
    extension [T](x: T)
 
      def ensuring2(condition: WrappedResult[T] ?=> Boolean): T =  
        assert(condition(using x))
        x
       
  end PostConditions

  @main def context_functions_start(): Unit = {
    
    val r = f(3)(using ec)
    val r2 = f2(3)
    val r3 = f3(3)
    val r4 = f4(3)

    println(f"r4 is $r4")

    val r5 = g(22) // is expanded to g( (ev: ExecutionContext) ?=> 22  )

    val r6 = g(f(2)) // is expanded to g( (ev: ExecutionContext) ?=> f(2)(using ev) )

    val r7 = g( (ctx: ExecutionContext) ?=> f(3) ) // is expanded to g( (ctx: ExecutionContext) ?=> f(3)(using ctx)   )

    val r8 = g( (ctx: ExecutionContext) ?=> f(3)(using ctx) ) // is left as it is 

    println(f"r5: $r5, r6: $r6, r7:$r7 r8:$r8 ")

    val r9 = table { ($t: Table) ?=> 
        row { ($r: Row) ?=>
          cell("top left")
          cell("top right")
        }

        row { ($r: Row) ?=>
          cell("bottom left")(using $r)
          cell("bottom right")(using $r)
        }(using $t)

        row { r ?=>
          cell("kkk")
          cell("kkk2")
        }
    }

    println(f"r9 is $r9")
    
    
    import PostConditions._ 
    
    val s = List(1, 2, 3).sum.ensuring2(result == 6)
    println(f"s is $s")

    val s2 = List(1, 2, 3).sum.ensuring2((wp: WrappedResult[Int]) ?=>  result(using wp) == 6) // ensuring2( (wp: WrappedResult[Int])  => result(using wp) == 6  )
    println(f"s is $s2")

    val s3 = List(1, 2, 3).sum.ensuring2((wp: WrappedResult[Int]) ?=>  result == 6) // ensuring2( (wp: WrappedResult[Int])  => result(using wp) == 6  )
    println(f"s is $s3")
    
    
    
  }

}
