package dotty_new.tools.dot.util

import scala.collection.mutable.ArrayBuffer

// A character buffer that exposes the internal array for reading
class CharBuffer(initialSize: Int = 1024) {
  private val buffer = ArrayBuffer.empty[Int]
  
  def add(v: Int): Unit = {
    buffer += v 
    
  }
}

object CharBuffer:

  val r = 0



