package dotty_new.tools.dot.util
import scala.language.strictEquality

object eq_demo {
  
  
  class A(int: Int) derives CanEqual:
    val init_value = int


    override def equals(obj: Any): Boolean = this match
      case o: A => println("kk"); init_value == o.init_value
      case null => false 
   
  class B(int: Int) extends A(int):
    override def equals(obj: Any): Boolean = this match
      case o: B => println("jj"); init_value == o.init_value
      case null => false
  
  
  class Box[T](x: T) derives CanEqual

  given CanEqual[C, D] = CanEqual.derived
  
  class C(int: Int) derives CanEqual
  class D(int: Int)
  
  case class CC()
  case class DD()
  
  trait FF:
    def showff(): Unit = {
      println("ff")
    }
  
  trait GG:
    def showgg(): Unit = {
      println(s"gg")
    }
  
  class AAA extends FF, GG 
  

  @main def eq_demo_start(): Unit = {
    
    val aaa = AAA() 
    aaa.showgg()
    aaa.showff()
    
    val r = A(3)
    val r2 = B(3)
    
    println(r == r2)
//    println(r2 == r)
    println(r eq r2 )
    
    println(s"box instances ...")
    val b1 = Box(2)
    val b2 = Box(2)
    
    println(b1 == b2)
    
    val c = C(4)
    val d = D(4)
    println(c == c )
    println(c == d )
    
    val rr =
      val r = 300
      r
    end rr 
     
    
    val r4 = "_root_"
    val s"_${r5}_" = r4 
    println(s"r5 is $r5")
    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    val LocalPrefix: String = "local"
    val PackageObjectDescriptor: String = "package."
    val s"${RootPackageName @ _}/" = RootPackage
    val s"${EmptyPackageName @ _}/" = EmptyPackage
    println(RootPackageName)
    println(EmptyPackageName)
    
     
  }

}
