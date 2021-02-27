package scala3_new_syntax_demo

import scala.util.{Failure, Try}

object little_collection_demo {
  
  
  case class Point[A](x: A, y: A, z: A)
  

  abstract class WithFilter[+A, CC[_]]:
    def map[B](f: A => B): CC[B]
    def flatMap[B](f: A => CC[B]): CC[B]
    def foreach[U](f: A => U): Unit 
    def withFilter(f: A => Boolean): WithFilter[A, CC]
  
  
  enum Maybe[+A]:
    self =>
    case Empty
    case Just(v: A)
     
    def map[B](f: A => B): Maybe[B] = this match
      case Just(v) => Just(f(v))
      case Empty => Empty 
  
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match
      case Just(v) => f(v)
      case Empty => Empty
    
    def filter(p: A => Boolean): Maybe[A] = this match
      case pp@Just(v) if p(v) => pp
      case _ => Empty 
    
    def foreach[U](f: A => U): Unit = this match
      case Just(v) => f(v)
      case _ => ()
      
    
    def withFilter(p: A => Boolean): WithFilter[A, Maybe] = WF(p)
    
    // 这下面其实可以是看成代理对象的逻辑
    class WF(p: A => Boolean) extends WithFilter[A, Maybe]:
      
      def filter: Maybe[A] = self filter p 
      
      override def map[B](f: A => B): Maybe[B] = filter map f 

      override def flatMap[B](f: A => Maybe[B]): Maybe[B] = filter flatMap f 

      override def foreach[U](f: A => U): Unit = filter foreach f 

      override def withFilter(f: A => Boolean): WithFilter[A, Maybe] = WF(e => p(e) && f(e))
    
    
//  
//    class WithFilter(p: A => Boolean):
//      def map[B](f: A => B): Maybe[B] = self filter p map f 
//      def flatMap[B](f: A => Maybe[B]): Maybe[B] = self filter p flatMap f 
//      def withFilter(pp: A => Boolean): WithFilter = WithFilter(e => p(e) && pp(e))
  
  object Maybe:
    def maybe[A](v: A): Maybe[A] = Just(v)
    def empty[A]: Maybe[A] = Empty 
    
  
  def parse[A](v: A): Try[A] = Try(v)
  def failure[A]: Failure[A] = Failure(Exception("kkk"))

  @main def little_collection_demo_start: Unit = {
    val r = Maybe.maybe(4)
    val r2 = Maybe.maybe(5)
    
    val r3 = 
      for 
        a <- r 
        if a == 4
        b <- r2 
        if b == 5
      yield 
        a + b 
    
    println(f"r3 is $r3")
    
    r3.foreach(println)
    
  }
}
