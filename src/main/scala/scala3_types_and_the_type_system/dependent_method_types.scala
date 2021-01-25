package scala3_types_and_the_type_system

import scala3_book.ch12_multiveral_equality.PrintedBook

object dependent_method_types {
  
  trait Key:
    type Value 
  
  type DB = (k: Key) => Option[k.Value]
//  trait DB:
//    def get(k: Key): Option[k.Value] // a dependent method
   
  object Name extends Key:
    type Value = String 
  
  object Age extends Key:
    type Value = Int 
  
  // a user of a DB 
  def user(db: DB): Tuple = {
    println(f"我们可以使用高级类型")
    (db(Name), db(Age))
  }
  
  trait Nums:
    // the type of numbers is left abstract
    type Num 
    // some operations on numbers
    def lift(d: Double): Num 
    def add(l: Num, r: Num): Num 
    def mul(l: Num, r: Num): Num 
    def Double(num: Num): Double = 0.0
  
  trait NumsDSL extends Nums:
    extension (x: Num)
      def +(y: Num) = add(x, y)
      def *(y: Num) = mul(x, y)
      def toDouble: Double = Double(x)
  
  def const(d: Double)(using n: Nums): n.Num = n.lift(d)
  
  given numsIntDSL: NumsDSL with
    type Num = Int 
    def lift(d: Double): Num = d.toInt
    def add(l: Num, r: Num): Num = l + r 
    def mul(l: Num, r: Num): Num = l * r 
    
  
  type Prog = (n: NumsDSL) ?=> n.Num => n.Num 
  
  def derivative(input: Prog)(v: Double)(using ev: NumsDSL): Double = input(const(v)).toDouble

  @main def dependent_method_types_start(): Unit = {
    
//    val db: DB = DB 
//    val res1: Option[String] = db.get(Name)
//    val res2: Option[Int] = db.get(Age)
 
     
//    val r = derivative(x => x)(0.3)(using numsIntDSL)
    val r = derivative(x => x)(0.3)
    println(f"r is $r")
    
  }

}
