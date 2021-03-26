package scala3_datastructre
import collection.mutable.{Queue, Stack}
import scala.collection.mutable

// 状态不变性,从一个状态转换为另一个状态!!!
type ST = [S, A] =>> S => (A, S)

// derives clause generates the following given istances for Eq, Ordering and Show type classes in 
// the companion object of Tree.

//sealed trait Mirror:
//  type MirroredType
//  type MirroredElemTypes 
//  type MirroredMonoType 
//  type MirroredLabel <: String 
//  type MirroredElemLabels <: Tuple 
//
//object Mirror:
//  trait Product extends Mirror:
//    def fromProduct(p: scala.Product): MirroredMonoType
//
//  trait Sum extends Mirror:
//    def ordinal(x: MirroredMonoType): Int 
//
//end Mirror 



enum Tree[T] : 
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)

//import Tree.* 
//// compiler generate 
//// Mirror for Tree
//val r = new Mirror.Sum {
//  override def ordinal(x: MirroredMonoType): Int = x match
//    case _: Branch[_] => 0
//    case _: Leaf[_] => 1 
//
//  override type MirroredType = Tree
//  override type MirroredElemTypes[T] = (Branch[T], Leaf[T])
//  override type MirroredMonoType = Tree[_]
//  override type MirroredLabel = "Tree"
//  override type MirroredElemLabels = ("Branch", "Leaf")
//}
//
//// Mirror for Branch 
//val r2 = new Mirror.Product {
//  override def fromProduct(p: Product): MirroredMonoType = Branch(???)
//
//  override type MirroredType = Branch
//  override type MirroredElemTypes[T] = (Tree[T], Tree[T])
//  override type MirroredMonoType = Branch[_]
//  override type MirroredLabel = "Branch"
//  override type MirroredElemLabels = ("left", "right")
//}
//
//// Mirror for Leaf 
//val r3 = new Mirror.Product {
//  override def fromProduct(p: Product): MirroredMonoType = Leaf(???)
//
//  override type MirroredType = Leaf
//  override type MirroredElemTypes[T] = Tuple1[T]
//  override type MirroredMonoType = Leaf[_]
//  override type MirroredLabel = "Leaf"
//  override type MirroredElemLabels = ("elem")
//}



  
object datastructure_demo {
  
  class Data[T](init: T):
    var _e: T = init
    def e: T = _e 
    def e_= (v: T) = _e = v 
  
  def convert_in_stack(n: Int): Unit = {
    val s = mutable.Stack.empty[Int]
    val store = Data(n)
    
    while store.e > 0 do 
      s.push(store.e % 2)
      store.e /= 2  
    
    while s.nonEmpty do 
      print(s.top)
      s.pop()
    
  }
  def convert(n: Int): Unit = {
    val rs = LazyList.unfold((n, List.empty[Int])){ case (s, t) => {
      if s == 0 then 
        None
      else { 
        val tmp = s % 2 :: t 
        Some((tmp, (s / 2, tmp)))
      }
    }}
    
    rs.last.foreach(print)
    println()
  }
  
  def stack_test(): Unit = {
    convert(300)
    convert_in_stack(300)
  }
  
  def dqueue_test(): Unit = {
   
  }
  
  def quque_test(): Unit = {
    val q = Queue.empty[Int]
//    q.enqueue(3)
//    q.enqueue(4)
    q.enqueueAll(1 to 10)
    println(s"q is $q")
    println(s"q's head is ${q.front}")
   
    
  }
  
  def vector_test(): Unit = {
    val v = (1 to 10).toVector
    println(s"v is $v v(0) is ${v(0)}")
  }
  
  def multi_map_test(): Unit = {
    val r = (1 to 10).map(e => (e, e)).toMap.groupBy[Int]((k, v) => k % 2)
    println(s"r is $r")
    
    val r2 = (1 to 10).groupBy(e => e % 3).keys
    println(s"r2 is $r2")
  }
  
  
  @main def datastructure_demo_start(): Unit = {
    vector_test()
    quque_test()
    stack_test()
//    multi_map_test()
  }
}
