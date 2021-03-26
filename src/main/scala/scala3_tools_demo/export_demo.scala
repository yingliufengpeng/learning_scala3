package scala3_tools_demo

class MyBuffer:
  val sb = StringBuffer() 
  export sb.append 
  export sb.{insert => putIn}

  override def toString: String = sb.toString 

def exportClauses() =
  val buffer = MyBuffer() 
  buffer.append("Foo")
  buffer.append("Bar")
  buffer.putIn(3, "Wibble")
  println(s"$buffer")
  

@main def export_demo_start(): Unit = {
  exportClauses()
}