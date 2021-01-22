package scala3_book
import scala.language.strictEquality

// But with Scala3 you can disable such comparisons. By importing scala.language.strictEquality 
// or using the language:strictEquality compiler flag, this comparision no long compiles.

object ch12_multiveral_equality {
  // Option 1
  case class Cat(name: String) derives CanEqual
  case class Dog(name: String) derives CanEqual

  // Option 2 
  case class Dog2(name: String) 
  given CanEqual[Dog2, Dog2]  = CanEqual.derived 
  
  trait Book:
    def author: String 
    def title: String 
    def year: Int 
  
  case class PrintedBook(
                        author: String,
                        title: String ,
                        year: Int,
                        pages: Int
                        ) extends Book

  case class AudioBook(
                          author: String,
                          title: String ,
                          year: Int,
                          lengthInMinutes: Int
                        ) extends Book:

    override def equals(that: Any): Boolean = that match
      case a: AudioBook =>
          if this.author == a.author
            && this.title == a.title 
            && this.year == a.year
            && this.lengthInMinutes == a.lengthInMinutes
              then true else false
  
      case p: PrintedBook =>
          if this.author == p.author && this.title == p.title 
            then true else false
  
      case _ => false
    

  given CanEqual[PrintedBook, PrintedBook] = CanEqual.derived 
  given CanEqual[AudioBook, AudioBook] = CanEqual.derived 
  
  given CanEqual[AudioBook, PrintedBook] = CanEqual.derived
  given CanEqual[PrintedBook, AudioBook] = CanEqual.derived
  
  @main def multiveal_equality_start(): Unit = {
    
    val d = Dog("Fido")
    val c = Dog("Morris")
    
    println(d == c)
    
    val d2 = Dog2("Frido")
    val c2 = Dog2("Morris")
    
    println(d2 == c2)
    
    val p1 = PrintedBook("1984", "George Orwell", 1961, 328)
    val p2 = PrintedBook("1984", "George Orwell", 1961, 328)
    
    println(p1 == p2)
    
    val pBook = PrintedBook("1984", "George Orwell", 1961, 328)
    val aBook = AudioBook("1984", "George Orwell", 2006, 628)
    
    println(aBook == pBook)
    println(pBook == aBook)
    
  }
  
}
