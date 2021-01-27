package scala3_other_new_feathures

object safe_initialization {
  
  /// 未完待续,以后有机会再去看这其中的意义
  
  abstract class AbstractFile:
    def name: String 
    val extension: String = name.substring(4)
  
  class RemoteFile(url: String) extends AbstractFile:
    val localFile: String = f"${url.##}.tmp"
    def name: String = localFile
  
  object Trees:
    class ValDef {_counter += 1}
    class EmptyValDef extends ValDef 
    val theEmptyValDef = EmptyValDef() 
    private var _counter: Int = 0
    def counter = _counter
  
  
  abstract class Parent:
    val f: () => String = () => this.message 
    def message: String 
  
  class Child extends Parent:
    val a = f()
    val b = "hello"
    override def message: String = b
  
  @main def safe_initialization_start(): Unit = {
    
//    val r = RemoteFile("kk")
//    println(f"r is ${r.name}")
    
    Trees.theEmptyValDef
//    val r = Trees.EmptyValDef() 
    println(Trees.counter)
    
   }
}
