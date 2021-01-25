package scala3_types_and_the_type_system

object structural_types {
  
  
  class MyRecord(elems: (String, Any)*) extends Selectable:
    private val fields = elems.toMap 
    def selectDynamic(name: String): Any = fields(name)
    
  
  // In practice, the connection between a structual type and its underlying generic representation would 
  // most likely be done by database layer, and therefore would not be a concern of the end user.
  type Person = MyRecord {
    val name: String 
    val age: Int
  }
  
  type Book = MyRecord {
    val title: String
    val author: String
    val year: Int 
    val rating: Double 
  }
  
  @main def structural_types_start(): Unit = {
    val person = MyRecord(
      "name" -> "Emma",
      "age" -> 42
    ).asInstanceOf[Person]
    
    println(f"${person.name} is ${person.age} years old")

    val book = MyRecord(
      "title" -> "The Catcher in the Rye",
      "author" -> "J. D. Salinger",
      "year" -> 1951,
      "rating" -> 4.5
    ).asInstanceOf[Book]
    
    println(f"book is $book")
  }

}
