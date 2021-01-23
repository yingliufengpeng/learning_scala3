package scala3_scala_for_python_developer

object for_python_developer {
  
  
  @main def python_developer_start(): Unit = {
    
    val movies = Map(
      "Toy Story" -> 8.3,
      "Forrest Gump" -> 8.8,
      "Cloud Atlas" -> 7.4
    )
    
    val x = Set(1, 2, 3)
    
    val x2 = (11, "Eleven")
    
    class Person(var name: String): 
      def speak = println(f"Hello, my name is $name")
    
    val p = Person("John")
    
    def add(a: Int, b: Int): Int = a + b 
    
    def walkThenRun() = {
      println(f"Walk")
      println(f"run")
    }
    
    val x5 = 1
    if x5 == 1 then println(x5)
    
    if x5 == 1 then { 
      println(f"x5 is 1, as you can see:")
      println(x5)
    }
    
    
    if x5 < 0 then 
      println("negative")
    else if x5 == 0 then 
      println("zero")
    else
      println("positive")
    
    def min(a: Int, b: Int): Int =
      if a < b then a else b 
    
    var i = 1
    while i < 3 do 
      println(i)
      i += 1
    
    // prefered 
    for i <- 0 until 3 do println(i)
    
    // also avaliable 
    for (i <- 0 until 3) println(i)
    
    // mutliline syntax
    for 
      i <- 0 until 3
    do 
      println(i)
    
    val ints = List(3, 4, 5)
    for i <- ints do println(i)
    
    for 
      i <- ints
    do 
      println(i)
      
    for 
      i <- 1 to 2
      j <- 4 to 5
      k <- 1 until 10 by 3 
    do 
      println(f"i = $i, j = $j, k = $k")
    
    
    for 
      i <- 1 to 10
      if i % 2 == 0
      if i < 5
    do 
      println(i)
    
    for 
      i <- 1 to 10
      if i % 2 == 0 && i < 5
    do 
      println(i)
      
    
    val x6 = 
      for 
        i <- 1 to 3 
      yield 
        i * 10
    

  }
}
