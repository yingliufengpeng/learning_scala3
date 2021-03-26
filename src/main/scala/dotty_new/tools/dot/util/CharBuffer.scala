package dotty_new.tools.dot.util

import scala.io

import dotty.tools.dotc.parsing.CharArrayReader

class AB:
  val r = 3

class MyCharArrayReader extends CharArrayReader :
  val buf = io.Source.fromFile("./celsius.txt").toArray

  override protected def decodeUni: Boolean = true

  override protected def error(msg: String, offset: Int): Unit = {}


def test2(): Unit = {
  val reader = MyCharArrayReader()
  println(s"reader is $reader")
  (1 to 20).foreach(_ => print(reader.getc()))

}

def test(): Unit = {
  val a = AB()
  println(s"a is $a")
}

@main def myCharArrayReader_start2(): Unit = {


  test()
  test2()

}





