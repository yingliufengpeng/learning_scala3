package scala3_book

object ch11_type_class {

  // In Scala3, type classes are just traits with one or more parameterss whose implentations 
  // are provided by given instances
  
  // a type class  This is the Scala3 way of saying that any type that implements this 
  // triat must definde how the show method works. Notice that the syntax is very close 
  // to a normal trait 
  trait Showable[A]:
    extension (a: A) def show: String 
  
  // a trait 
  trait Show:
    def show: String 
  
  // 1 Type-classes like Showable take a type parameter A to say which type we provide the 
  // implementation of show for; in contrast, normal traits like Show do not 
  
  // 2 To add the show functionality to a certain type A, the normal trait requires that 
  // A extends Show, while for type-claesses we require to have an implementation of Showable[A]
  
  // 3 To allow the same method calling syntax in both Showable that mimics the one of show, 
  // we define Showable.show as an extension method 
  
  case class Person(firstName: String, lastName: String)
  
  given Showable[Person] with
    extension (p: Person) def show: String = f"${p.firstName} ${p.lastName}"
  
  // As shown, this is defined as an extension method on the Person class, and it uses the 
  // reference p inside the body of the show method 
  
  
  def showAll[S: Showable](xs: List[S]): Unit =
    xs.foreach(x => println(x.show))
    
  
  trait HasLegs[A]:
    extension (a: A)
      def walk(): Unit 
      def run(): Unit 
  
  @main def type_class_start(): Unit = {
    
    val person = Person("Wang", "Peng")
    println(f"person is ${person.show}")
    
    showAll(List(Person("冰冰", "狗"), Person("ice_ice_", "dog")))
  }
}
