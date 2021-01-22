package scala3_book.ch14_open_class_demo.open

import scala3_book.ch14_open_class_demo.open.Writer

class EncryptedWriter[T] extends Writer[T]:
  // 做一次加密的处理
  override def send(x: T): Unit = super.send(x)


// An open class typically comes with some documentation that describle the internal calling
// patterns between methods of the class as well as hooks that can be overridden. We call this 
// extension contract of the class. It is different from the external contract between a class 
// and its users 

// Classes that are not open can still be extended, but only if at least one of two alternative 
// conditions is met:
//  The extending class is in the same source file as the extended class. In this case, 
//    the extension is usually an internal implementation matter.
//
//  The language feature adhocExtensions is enabled for the extending class. This is typically
//    enabled by an import clause in the source file of the extension:
//    import scala.language.adhocExtensions

object encrypetd_writer:

  @main def encrypetd_writer_start(): Unit = {
    val r = EncryptedWriter[Int]()

    r.send(33)
  }
