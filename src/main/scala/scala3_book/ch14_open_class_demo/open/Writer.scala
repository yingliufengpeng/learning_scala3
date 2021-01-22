package scala3_book.ch14_open_class_demo.open

open class Writer[T]:
  // 
  def send(x: T) = println(x)

  def sendAll(xs: T*) = xs.foreach(send)


