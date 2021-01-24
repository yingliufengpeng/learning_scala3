package owl

import javax.swing.event.DocumentEvent.EventType
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


type Callback = (args: Tuple) => Unit
 

case class Subscription(owner: Any, callback: Callback)

class EventBus:
   
  val subscriptions: Map[String, ArrayBuffer[Subscription]] = Map.empty

  def on(eventType: String, owner: Any, callback: Callback): Unit = {
    this.subscriptions
      .getOrElseUpdate(eventType, ArrayBuffer.empty)
      .addOne(Subscription(owner, callback))
  }

  def off(eventType: String, owner: Any): Unit = {
    val r = this.subscriptions
      .getOrElseUpdate(eventType, ArrayBuffer.empty)
      .filterInPlace(s => s.owner != owner)
  }

  def trigger(eventType: String, args: Tuple): Unit = {
    this.subscriptions
      .getOrElseUpdate(eventType, ArrayBuffer.empty)
      .foreach(s => {
        println(f"s is $s,   owner is ${s.owner}  args is $args")
        s.callback(s.owner, args)
      })
  }

  def clear(): Unit = {
    this.subscriptions.empty
  }


object tests {
  
  enum Color2:
    case Color(r: Byte, g: Byte, b: Byte)
    case HSI(h: Int, s: Int, i: Int)
    
    def eat: Unit = this match
      case Color(r, g, b) => f""
      case HSI(h, s, i) => f""
  
  extension (c: Color2)
     
    def show: String = c match
      case Color2.Color(r, g, b) => f"r: $r, g: $g, b: $b"
      case Color2.HSI(h, s, i) => f"h: $h, s: $s, i: $i"
   
  val r: Int | Null = null 
  val r2: Option[Int] = Some(4) 
  val r3 = r match
    case p: Int => p
    case _ => 0
    
  val r4 = r2.getOrElse(0)
   
  def ff[A, B, C](x: A, y: B, z: C): Tuple = (x, y, z)
   
  @main def tests_start(): Unit = {

    val r: Callback = (args) => {
      args.productIterator.foreach(println)
      println(f"args is $args class is ${args.getClass}")

    }

    val r2 = (1, 2, 3, 4)

    //    r(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, Object(), 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 )


    val eventBus = EventBus()

    eventBus.on("change", null, e => {
      println(f"e is $e")
    })
    println(f"s is ${eventBus.subscriptions}")

    eventBus.trigger("change", (1, 3, "4"))

    eventBus.off("change", null)

    eventBus.trigger("change", (1, 3, "4"))
    
    
    val r10: Any = null 
    val r11 = null 
    def f[T](x: T) = println(f"x is $x")
    
    f(null)
    
    val r12: Tuple = ff(3, Object(), "4")
    println(f"r12 is $r12")
    

  }


}
 
