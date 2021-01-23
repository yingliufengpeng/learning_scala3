package scala3_book

object ch20_inner_classes_abstract_type_members {
  
  class Graph:
    type CommonNode = Graph#Node
    class Node:
      var connectedNodes: List[CommonNode] = Nil 
      def connectTo(node: CommonNode): Unit =  
        if (!connectedNodes.exists(node.equals)) 
          then 
            connectedNodes = node :: connectedNodes
    
    var nodes: List[Node] = Nil 
    
    def newNode(): Node =
      val res = Node()
      nodes = res :: nodes
      res 
  
  trait Buffer:
    type T 
    val element: T 
  
  abstract class SeqBuffer extends Buffer:
    type U 
    type T <: Seq[U] 
    def length: Int = element.length
  
  abstract class IntSeqBuffer extends SeqBuffer:
    type U = Int 
  
  def newIntSeqBuf(elem1: Int, elem2: Int): IntSeqBuffer =
    new IntSeqBuffer {
      override type T = List[U]
      override val element: T = List(elem1, elem2)
    }

  // Note that we have to use variance annotations here (+T <: Seq[U]) in order to 
  // hide the concrete sequence implementation byte of the obejct returned from method
  // newIntSeqBuf2. Furthermore, there are cases where it is not possible to replace 
  // abstract type members with type parameters.
  abstract class Buffer2[+T]:
    val element: T 
  
  abstract class SeqBuffer2[U, +T <: Seq[U]] extends Buffer2[T]:
    def length: Int = element.length
  
  def newIntSeqBuf2(e1: Int, e2: Int): SeqBuffer2[Int, Seq[Int]] =
    new SeqBuffer2[Int, List[Int]] {
      val element = List(e1, e2)
    }
  
  
  
  
   
  @main def inner_classes_abstract_type_members_start(): Unit = {
    val graph1 = Graph() 
    val node1 = graph1.newNode()
    val node2 = graph1.newNode() 
    val node3 = graph1.newNode() 
    
    node1.connectTo(node2)
    node2.connectTo(node1)
    
    println(f"node1 is $node1")
    println(f"node2 is $node2")
    println(f"node3 is $node3")
    
    val graph2 = Graph() 
    val node4 = graph2.newNode()
    node4.connectTo(node4)
    
    node1.connectTo(node4)
    
    println(f"node4 is $node4")
     
    val buf = newIntSeqBuf(7, 8)
    println(f"length = ${buf.length}")
    println(f"content = ${buf.element}")
    
    
   
  
  }

}
