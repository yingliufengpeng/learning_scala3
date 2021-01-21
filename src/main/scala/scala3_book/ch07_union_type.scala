package scala3_book

 
object ch07_union_type {

  case class Username(name: String)
  case class Password(hash: String)
  
  def help(id: Username | Password) =
    val `val` = id match {
      case Username(name) => name
      case Password(hash) => hash 
    }
    println(f"val is ${`val`}")
  
  @main def union_type_start(): Unit = {
    
    val u = Username("映柳枫鹏")
    
    help(u)
  }
}
