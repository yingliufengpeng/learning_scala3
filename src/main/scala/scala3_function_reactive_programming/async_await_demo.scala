package scala3_function_reactive_programming
 
object async_await_demo {
  
  def event_driven[T, U](t: T, f: T => U): U =
    f(t)
    
  class Counter:
    private var counter = 0
    
    def add(v: Int): Unit =
      counter += v 
      
  // Use Scaling out: distributed actors
  enum Json:
    case JSeq(elems: List[Json])
    case JObj(bindings: Map[String, Json])
    case JNum(num: Double)
    case JStr(str: String)
    case JBool(b: Boolean)
    case JNull
    
    // 首先是要弄明白这个结构的数据是属于谁
    // isInList 用于判断是否位于object对象中,否则就是在[]列表对象之中
    def prettyShow(level: Int = 0, isInList :Boolean = false): String = {
      val Spaces = " "
      this match
        case JObj(bindings) =>
          val firstPrefix = if isInList then Spaces * level + "{\n" else "{\n"
          val middile = bindings.init.map((k, v) => Spaces * (level + 1) + "\"" + k + "\": " + v.prettyShow(level + 1) + ",\n").mkString("")
          val last_middile = (bindings.last :: Nil).map((k, v) => Spaces * (level + 1) + "\"" + k + "\": " + v.prettyShow(level + 1) + "\n").mkString("")
          val lastPrefix = Spaces * (level) + "}"
          firstPrefix + middile + last_middile + lastPrefix
        case JSeq(elems) =>
          val firstPrefix = "[\n"
          val middile = elems.map(e => e.prettyShow(level + 1, true) + ",\n").mkString("")
          val lastmiddile = (elems.last :: Nil).map(e => e.prettyShow(level + 1, true) + "\n").mkString("")
          val lastPrefix = Spaces * level + "]"
          firstPrefix + middile + lastmiddile + lastPrefix
        case JNum(num) => s"$num"
        case JStr(str) => "\"" + str + "\""
        case JBool(b) => s"$b"
        case JNull => s"null"
    }
 
  
  val f: PartialFunction[String, Int] = {
    case "pie" => 2
  }
  
  val f2: PartialFunction[List[Int], String] =
    case Nil => "zero"
    case x :: y :: rest => "two"
  
  @main def async_await_demo_start(): Unit = {
    val r = event_driven(3, e => e * 30)
    import Json._
    val data = JObj(Map(
      "firstName" -> JStr("John"),
      "lastName" -> JStr("Smith"),
      "address" -> JObj(Map(
        "streetAddress" -> JStr("21 2nd street")
      )),
      "phoneNumbers" -> JSeq(List(
        JObj(Map(
          "type" -> JStr("home"),
          "num" -> JNum(44),
        )),
        JObj(Map(
          "type" -> JStr("fax"),
          "kk" -> JSeq(List(
            JObj(Map(
              "m" -> JStr("kk")
            ))
          ))
        ))
      ))
    ))
    
    println(s"${data.prettyShow()}")
    
//    val r4 = f.lift("44").getOrElse(0)
//    println(s"r4 is $r4")
//    
//    val r5 = f2.lift(List(3, 4)).getOrElse(0)
//    println(s"r5 is $r5")
    
    val r10 = Map(1 -> 2, 3 -> 4, 5 -> 6)
//    r10.zipWithIndex.map((k, v), i) => (i, k, v )
  }

}
