package scala3_book

object ch09_given_instance_and_using_caluse {
  
  case class Config(port: Int, baseUrl: String)
  
  def renderWebsite(path: String, c: Config): String =
    f"<html> ${renderWidget(List("cart"), c)}"
  
  def renderWidget(items: List[String], c: Config): String = "ok"
  
  
  def renderWebsite2(path: String)(using c: Config): String =
    f"<html> ${renderWidget2(List("cart"))}"
    
  def renderWidget2(items: List[String])(using  c: Config): String = "ok"
  
  
  def f2(): Unit = {
    
    given Config = Config(8088, "wangpeng")
    val r = renderWebsite2("/home") 
    
    println(f"in f2 r is $r")
  }
  
  @main def given_instance_and_using_caluse_start(): Unit = {
    
    val config = Config(8088, "docs.scala-lang.org")
    
    val r = renderWebsite2("/home")(using config)
    
    println(f"r is $r")
    
    f2()
    
    
  }

}
