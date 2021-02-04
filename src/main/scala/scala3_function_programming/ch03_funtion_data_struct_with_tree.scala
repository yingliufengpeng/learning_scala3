package scala3_function_programming

object ch03_funtion_data_struct_with_tree {
  
  enum MyTree[+A]:
    case Leaf(v: A)
    case Branch(left: MyTree[A], right: MyTree[A])
    
    
    def fold[B](zero: B)(f: (B, A) => B)(com: (B, B) => B): B = this match
      case Leaf(v) => f(zero, v)
      case Branch(left, right) => com(left.fold(zero)(f)(com), right.fold(zero)(f)(com))
     
    def size: Int = this match
      case Leaf(_) => 1
      case Branch(left, right) => left.size + right.size + 1
    
    
    def size_fold: Int = fold(0)((acc, _) => acc + 1)(_ + _ + 1)
  
    
    def depth: Int = this match
      case Leaf(_) => 1
      case Branch(left, right) => (left.depth max right.depth) + 1
    
    
    def depth_fold: Int = fold(0)((acc, _) => acc + 1)(_ max _ + 1)
  
    
    def map[B](f: A => B): MyTree[B] = this match
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(left.map(f), right.map(f))
  
  
  object MyTree:
    def apply[A](h: A, as: A*): MyTree[A] = as.foldLeft(Leaf(h))((acc, v) => Branch(Leaf(v), acc))
  
    def maximum(t: MyTree[Int]): Int = t match
      case Leaf(v) => v
      case Branch(left, right) => maximum(left) max maximum(right)
  
    def maximum_fold(t: MyTree[Int]): Int = t.fold(0)((acc, v) => acc max v)(_ max _)
  
  
  @main def tree_start(): Unit = {
    val r = MyTree(7, 3, 5)
    println(f"r'tree is $r")
    println(f"r's size is ${r.size}")
    println(f"r's size_fold is ${r.size_fold}")
    
    val r2 = MyTree.maximum(r)
    println(f"r2 is $r2")
    val r22 = MyTree.maximum_fold(r)
    println(f"r22 is $r22")
    
    val r3 = r.depth
    println(f"r3's depth is ${r.depth}")
    println(f"r3's depth_fold ${r.depth_fold}")
    
    val r4 = r.map(_ * 2)
//    val r44 = r.map_fold(0)(_ * 2)
    println(f"r4 is $r4")
//    println(f"r44 is $r44")
    
    val r5 = List(3, 4)
    val r6 = r5.fold(0)(_ + _)
    
  }

}
