package scala3_tools_demo

import scala3_tools_demo.demo2.LList.Head

import scala.io.BufferedSource

object demo2 {

  trait A
  trait B 
  trait MA:
    def get: A 
  
  trait MB:
    def get: B
   
   
//  type Container[t] = t match 
//    case v @ String => v
    
  
  
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
  
  // Scala 的case 语句读起来并不是很容易,或者说分支太多容易干扰人!!!
  // 如何避免scala本身case分支语句的影响这才能是自己最大的提升
  def f(xx: Int): Int =
    val x = xx match
      case 4 => 4
      case p if p % 2 ==0 => p match
        case 5 => 5
        case pp => pp match
          case 6 => 6
          case ppp => ppp
      case pp => pp  
    x 
  
  def test2(): Unit = {
    val r = f(4)
    val r2 = f(5)
    val r3 = f(6)

    println(f"r is $r r2 is $r2, r3 is $r3")

  }
    
   
  enum LList:
    case Head(ch: Char)  
    case Cons(left: LList, right: Head)
    
    def pretty_show(seq: Char = ','): String = this match
      case Head(v) => f"$v"
      case Cons(left, right) => f"${left.pretty_show(seq)}$seq${right.pretty_show(seq)}"
  
  object LList:
    def single(v: Char): Head = Head(v) 
    def cons(ts: LList, v: Char): LList = Cons(ts, single(v))
  
  
  enum Region:
    case Root
    case Paras(left: Char, right: Char, outer: Region) // ch '(' ')'  '[', ']'
    case Brace(outer: Region) 
    
    
    def show_left: String = this match
      case Root => ""
      case Paras(l, _, outer) => s"${outer.show_left}$l"
      case Brace(outer) => s"${outer.show_left}{"
    
    def show_right: String = this match
      case Root => ""
      case Paras(_, r, outer) => s"$r${outer.show_right}"
      case Brace(outer) => s"}${outer.show_right}"
    
    // this 则是代表当前可用的记录,关键在于如何把该记录进行
    // 创建自己所需要的状况!!! 
    // 树形结构,从下往上进行分析该问题,看来这个并不是所谓的递归可用的问题,应该是
    // 其他可以思考的问题
    def pretty_show: String = this.show_left + this.show_right
  
    def isOutest: Boolean = this == Root
    
    
  
  object Region:
    // ()()(){()}[()]
    def fromSring(input: io.Source): List[Region] = {
      // 双堆栈结构
      val stack = collection.mutable.Stack.empty[Char]
      val stack_regoin = collection.mutable.Stack[Region](Root)
      
      val buffer = collection.mutable.ListBuffer.empty[Region]
      
      input.toStream.foreach { e =>
        e match
          case '(' => val r = Paras('(', ')', stack_regoin.head); stack_regoin.push(r); stack.push('('); buffer += r; r.pretty_show
          case '[' => val r = Paras('[', ']', stack_regoin.head); stack_regoin.push(r); stack.push('['); buffer += r; r.pretty_show
          case '{' => val r = Brace(stack_regoin.head); stack_regoin.push(r); stack.push('{'); buffer += r; r.pretty_show
          case ')' | ']' | '}' => stack.pop(); stack_regoin.pop()
      }

      buffer.toList
    }
  
  def test4(): Unit = {
    val r = Region.fromSring(io.Source.fromString("()()[({()()})]"))
    r.foreach(e => println(e.pretty_show))
    
//    val r2 = Region.Paras('(', ')', Region.Root)
//    r2 match
//      case Region.Paras(l, r, _) => println(s"$l $r ")
//    
//    val r3 = r2.pretty_show
//    println(s"r3 is $r3")
  }

  //    def cons[T](head:T, t: LList[T]): LList[T] = Cons()
  
  def test3(): Unit = {
    val f = io.Source.fromFile("./celsius.txt")
    val r = f.toStream match
      case h #:: t => 
        lazy val left = LList.single(h)
        t.foldLeft[LList](left)((acc, e) => LList.cons(acc, e))
        
     
    println(f"r's result is ${r.pretty_show('-')}")
    
     
    
  }
  
  
  
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
//    val x: 1 | "2" = if true then 1 else "2"
//    
//    x match
//      case 1 => println(1)
//      case p => println(p)
    
    test4()
   
  }
}
