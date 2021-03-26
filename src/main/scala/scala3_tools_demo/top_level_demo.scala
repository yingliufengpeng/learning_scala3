package scala3_tools_demo

val menu =
  """
    |Pick an option:
    |0) Exit 
    |1) Creater applications
    |2) Extension methods
    |
    |""".stripMargin

def creatorApplication(): Unit = {
  println(s"app".times3)
}

def extensionMethod(): Unit = {
  println(s"extension")
}

extension  (s: String)
  def times3: String = s * 3 

def check(name: String | Null): String =  name match
  case s: String => s
  case _: Null => ""


def specailFunction(input: 1 | 2 | 3 ): Unit = input match
  case 1 => println(1)
  case 2 => println(2)
  case 3 => println(3)
  


@main def top_level_start(): Unit = {
  def loop: Unit = {
    println(menu)
    val choice = io.StdIn.readInt()
    choice match
      case 0 =>
      case 1 => creatorApplication(); loop
      case 2 => extensionMethod(); loop
      case _ => println(s"Try again"); loop 
    
  }
  loop
  
}
