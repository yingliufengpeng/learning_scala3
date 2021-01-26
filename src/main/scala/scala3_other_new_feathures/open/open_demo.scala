package scala3_other_new_feathures.open
import scala3_other_new_feathures.open.sub.Sub
object open_demo {

  
  @main def open_demo_start(): Unit = {
    val s = Sub()
    
    val xs = List((3, 4), (5, 6))
    
    val ys = xs.map((x, y) => x + y)
    println(f"ys is $ys")
    
    val zs = List(List(3, 4, 5), List(2, 3), List(1))
    
    val ws = zs.flatMap(list => list.map(e => e * 2))
    println(f"ws is $ws")
    
    val ws2 = for 
      list <- zs 
      e <- list 
    yield e * 2
    
    println(f"ws is $ws")
    println(f"ws is $ws2")
    
    
    for 
        list <- zs
        e <- list 
    do 
      println(f"e is $e")
   
    
    
  }
  
}
