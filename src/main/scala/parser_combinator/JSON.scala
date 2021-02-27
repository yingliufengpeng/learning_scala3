package parser_combinator

enum JSON:
  case JNUll
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

  def prettyShow: String = this match
    case JNUll => "null"
    case JNumber(v) => f"$v"  
    case JString(v) => "\"" + v :+ '\"'
    case JBool(v) => f"$v"
    case JArray(seq) => ""
    case JObject(map) => ""
