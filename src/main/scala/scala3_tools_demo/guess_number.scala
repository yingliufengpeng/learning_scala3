package scala3_tools_demo
import scala.io
import scala.util.{Success, Try}

case class Domain(term: (String, String, String), tails: Domain):
  def toDaminStr: String = ???

abstract class BasicModel[+T <: BasicModel[T]]:
  self =>
  
  def search(domain: Domain): List[T] = ???


object BasicModel:
  val r = 10



trait Common_Name(name: String)

case class User(name: String, login: String, password: String) extends BasicModel[User] with Common_Name(name)

case class Res_Company(name: String) extends BasicModel[Res_Company] with Common_Name(name)

object Res_Company:
  def search(domain: Domain): Option[List[Res_Company]] = ???

object guess_number {
  val the_secret_number = 100
  
  def loop: Unit = {
    println(f"please input your guessed number!")
    val s_number = Try { io.StdIn.readInt() }
    s_number match
      case Success(number) => 
        guess_number(number) match
          case true => 
          case _ => loop   
      case _ => println(f"输入错误,请重新输入"); loop
     
  }
  
  def guess_number(number: Int): Boolean = {  
    val compare = summon[Ordering[Int]]
    val r = compare.compare(number, the_secret_number)
    
    r match
      case 1 => println(f"bigger"); false
      case -1 => println(f"smaller"); false
      case _  => println(f"congratulation the same!"); true
   
  }


  def test(): Unit = {
    println(f"begin...")
    loop
  }

  
  @main def guess_number_start(): Unit = {
    test()
  }
}
