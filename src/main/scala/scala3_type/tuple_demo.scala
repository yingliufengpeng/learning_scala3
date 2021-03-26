package scala3_type


case class Region(row: Int, col: Int, outer: Region | Null):
  import Region.* 
  
  def show: String =
    val r = offset 
    println(s"======>")
    ""
   


object Region:
  val offset: (Int, Int) = (1, 5) // row, col 
    


@main def tuple_demo_start(): Unit = {
  (10, "x", true): (Int, String, Boolean)
  (10, "x", true) : (Int *: (String,  Boolean))
  
  
   
}
