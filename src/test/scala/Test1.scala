
import org.junit.Test
import org.junit.Assert._
 
enum Region:
  case A(outer: Region | Null)
  case B(outer: Region | Null)

enum Tree[T]:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(v: T)


class Test1 {
  val r = 10 
  var currentRegion = Region.B(null)
  
  var current_tree = Tree.Leaf(3)
  
  @Test def t1(): Unit = {
    val r = 10 
    println(s"r is $r")
    (1 to 10).foreach{ _ => currentRegion = Region.B(currentRegion)}
    println(s"current is $currentRegion")
    
    (1 to 10).foreach { _ => current_tree = Tree.Branch(current_tree, Tree.Leaf(3)) }
    println(s"current tree is $current_tree")
    
    (1 to 10).foreach { _ => 
      current_tree = current_tree match
        case Tree.Branch(l, _) => l
        case p => p 
    }
    println(s"current tree is $current_tree")
    
  }
 
}
 