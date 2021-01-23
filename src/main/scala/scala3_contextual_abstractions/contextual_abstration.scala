package scala3_contextual_abstractions

object contextual_abstration {
  
  // In scala3, type classes are just traits with one or more type 
  // parameters,like type parameters, like the following 
  
  trait Show[A]:
    def show(a: A): String
  
  class ShowInt extends Show[Int]:
    override def show(a: Int): String = f"The number is $a"
  
  def toHtml[A](a: A)(showA: Show[A]): String =
    f"<p>${showA.show(a)}</p>"
    
  
  // To make it more clear where givens in the current scope are coming from, a special form of the 
  // import statement is used to import given instance. The basic form is shown in this example 
  
  object A:
    class TC
    given tc: TC = TC()
    def f(using TC) = println(f"ok")
  
  
  object B:
    import A._  // import all non-given members
    
    val r = f
    println(f"r is $r")
    
    import A.given  // import the given instance 
    val r2 = f  
    println(f"r2 is $r2")
  
  
  // In this code the import A._ clause of object B imports all members of A except the given instance, tc. 
  // Conversely, the second import, import A.given, import only that given instance. The two import clause can 
  // also be merged into one:
  
  // object B:
  //    import A.{given, _}
  
  // These rules have two maini benefits
  // 1 It's more clear where givens in the current scope are coming from. In particular, it's not 
  //    possible to hide imported givens in a long list of other wildcard imports 
  //
  // 2 It enables importing all givens without importing anything else. This is important because givens 
  //    can be anoymous, so the usual use of named imports is not parctical.
  
  
  // a type class 
  trait Showable[A]:
    extension(a: A) 
      def show: String 
  
  
  case class Person(firstName: String, lastName: String)
  
  given Showable[Person] with
    extension(p: Person) 
      def show: String = f"${p.firstName} ${p.lastName}"
  
  
  def showAll[S: Showable](xs: List[S]): Unit =
    xs.foreach(x => println(x.show))
  
    
   
  @main def contextual_abstraction_start(): Unit = {
    
    
    
//    val r = toHtml(42)(ShowInt())
//    println(f"r is $r")
//    
    import B._
    println(f"r is $r")
    
    val person = Person("John", "Doe")
    val r4 = person.show
    println(f"r4 is $r4")
    showAll(List(person))
  }
}
