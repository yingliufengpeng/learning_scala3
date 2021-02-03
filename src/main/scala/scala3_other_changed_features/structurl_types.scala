package scala3_other_changed_features
import reflect.Selectable.reflectiveSelectable

object structurl_types {
  
  class Record(elems: (String, Any)*) extends Selectable:
    private val fields = elems.toMap 
    def selectDynamci(name: String): Any = fields(name)
  
  type Person = Record {
    val name: String 
    val age: Int
  }
  
  type Close = {
    def close(): Unit 
  }
 
  class FileInputStream: 
    def close: Unit ={
      
    }
  
  class Channel:
    def close(): Unit = {
      
    }
  
  def autoClose(f: Close)(op: Close => Unit): Unit =
    try op(f) finally f.close()
  
  trait Vehicle extends reflect.Selectable:
    val wheels: Int 
  
  
  
  @main def structural_types_strart(): Unit = {
    val i3 = new Vehicle {
        override val wheels: Int = 4
        val range = 40
    }
    
    println(f"i3 is ${i3.range}")
  }

}
