package scala3_tools_demo

import scala3_function_programming.ch10_monoids.Monoid

object opaque_types {
  
  trait SemiGroup[A]:
    extension (le: A)
      def combine(re: A): A 
      def |+| (re: A): A = le combine re 
  
  trait Monoid[A] extends SemiGroup[A]: 
    def zero: A 
  
  object Monoid:
    def apply[A](using m: Monoid[A]): Monoid[A] = m

    object AddInts:
      opaque type AddInt = Int

      object AddInt:
        def apply(v: Int): AddInt = v

      given intMonoidMult: Monoid[AddInt] with
        def zero = 0
        extension (le: AddInt)
          def combine(re: AddInt): AddInt = le + re
     
    
    object MulInts:
      opaque type MulInt = Int 
      
      object MultInt:
        def apply(v: Int): MulInt = v 
    
      given intMonoidMult: Monoid[MulInt] with
        def zero = 1
        extension (le: MulInt)
          def combine(re: MulInt): MulInt = le * re
    
    
    def fold[T](as: List[T])(using m: Monoid[T]): T =
      foldMap(as, identity)
      
    def foldMap[A, B: Monoid](as: List[A], f: A => B): B =
      val m = Monoid[B]
      as.foldLeft(m.zero)((acc, e) => acc |+| f(e))
    
    given listMonoid[T]: Monoid[List[T]] with 
      def zero = Nil 
      extension (le: List[T])
        def combine(re: List[T]): List[T] = le ++ re 
    
    given mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] with 
      val vm = summon[Monoid[V]]
      def zero: Map[K, V] = Map.empty
      extension (le: Map[K, V])
        // keys 把所有的key都已经回收回来
        def combine(re: Map[K, V]): Map[K, V] = (le.keys ++ re.keys).
          foldLeft(Map.empty[K, V])((acc, k) => 
            acc.updated( k, le.getOrElse(k, vm.zero) |+| re.getOrElse(k, vm.zero))
          )
    
    
    def test4(): Unit = {
      import AddInts.{given, _}
      val m1 = Map("a" -> AddInt(30), "b" -> AddInt(40))
      val m2 = Map("a" -> AddInt(3), "c" -> AddInt(4))
      
      val m3 = m1 |+| m2 
      println(s"m3 is $m3")
      
    }


    def test(): Unit = {
      import AddInts.{given, _}
      val r = AddInt(3)
      val r2 = AddInt(3)
      val r3 = AddInt(3)
      val rs = List(r, r2, r3)
      println(r |+| r2 |+| r3)
      println(fold(rs))
    }
  
    def test2(): Unit = {
      import MulInts.{given, _}
      val r = MultInt(10)
      val r2 = MultInt(10)
      val r3 = MultInt(10)
      
      val rs = List(r, r2, r3)
      println(r |+| r2 |+| r3)
      println(fold(rs))
      
    }
  
    def test3(): Unit = {
      import AddInts.{given, _}
      val r = (1 to 10).toList 
      val rs = foldMap(r, AddInt.apply)
      println(s"rs is $rs")
      
      val map = Map(3 -> 4, 5 -> 6)
      val map2 = map.updated(4, 4)
      
    }
  
  @main def opaque_types_start(): Unit = {
//    Monoid.test()
//    Monoid.test2()
//    Monoid.test3()
    Monoid.test4()
  }

}
