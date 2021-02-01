package scala3_function_reactive_programming

object Events {

  type MOVE = Point => Point
  type Move_Op = MOVE

  type Binary_Op = (left: Point, right: Point) => Point


  enum Tree[+T]:

    case Leaf(value: T)

    case Branch(left: Tree[T], right: Tree[T])

    def isLeaf(): Boolean = this match
      case Leaf(_) => true
      case _ => false

    def isBranch(): Boolean = !this.isLeaf()

    def toList(): List[T] = this match
      case Branch(left, right) => left.toList() ::: right.toList()
      case Leaf(v) => List(v)

  
    //  |__x
    //      |__x
    //      |   |__x
    //      |   |   |__x
    //      |   |   |   |__3
    //      |   |   |   |__4
    //      |   |   |__5
    //      |   |__6
    //      |__7
    def prettyPrint(): Unit =
      // Branch 也可能是lastChild, Leaf也可能是lastChild prefix: 上一级别所提供的的路径, is_lastChild当前的节点是否是父类节点的最后一个
      def inn(tree: Tree[T], prefix: String="", is_lastChild: Boolean=false): String = 
        tree match  
          case Branch(left, right) => 
            val new_prefix = s"${prefix}|__x\n"
            val new_child_prefix = s"${prefix}${if is_lastChild then "    " else "|   "}"
            val left_str = inn(left, new_child_prefix)
            val right_str = inn(right, new_child_prefix, true)
            s"$new_prefix${left_str}${right_str}"
          case Leaf(v) =>
            val new_prefix = s"${prefix}|__$v"
            s"$new_prefix\n"
     
      println(inn(this, is_lastChild = true))


  // [1, 2, 3, 4] ---> 
  object Tree:
    def apply[T](head: T, ts: T*): Tree[T] =
      ts.foldLeft(Leaf(head))((node, t) => Branch(node, Leaf(t)))


  case class Point(x: Int, y: Int)

  extension (left: Point)
    def bin_Action(binary_Op: Binary_Op)(right: Point) = binary_Op(left, right)


  def test(): Unit = {
    val left = Point(0, 0)
    val right = Point(1, 1)
  }


  @main def event_start(): Unit = {
    val r = Tree(3, 4, 5, 6, 7)
    println(f"r is $r")

    val r2 = r.toList()
    println(f"r2 is $r2")

    r.prettyPrint()


  }


}
