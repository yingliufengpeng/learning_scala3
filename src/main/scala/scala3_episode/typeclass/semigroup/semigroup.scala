package scala3_episode.typeclass.semigroup
import scala3_episode.typeclass.numeric.Scala3Numeric.Numeric

object semigroup {
  
  trait SemiGroup[A]:
    extension (fa: A)
      def combine(fb: A): A
      def |+| (fb: A): A = fa combine fb 
   
  object SemiGroup:
    def apply[A](using semi: SemiGroup[A]): SemiGroup[A] = semi
   
  
  trait Monoid[A] extends SemiGroup[A]:
    def zero: A 
  
  object Monoid:
    def apply[A](using mo: Monoid[A]): Monoid[A] = mo 
  
    object IntAdd:
      opaque type IntAdd = Int 
      
      def apply(v: Int): IntAdd = v 
      
      given Monoid[IntAdd]  with 
        def zero: IntAdd = 0
        extension (fa: IntAdd)
          def combine(fb: IntAdd): IntAdd = fa + fb

    object IntMul:
      opaque type IntMul = Int

      def apply(v: Int): IntMul = v

      given Monoid[IntMul] with
        def zero: IntMul = 1
        extension (fa: IntMul)
          def combine(fb: IntMul): IntMul = fa * fb
    
    given Monoid[String] with 
      def zero: String = ""
      extension (fa: String)
        def combine(fb: String): String = fa + fb 
    
    given mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] with 
      def zero: Map[K, V] = Map.empty
      extension (fa: Map[K, V])
        def combine (fb: Map[K, V]): Map[K, V] =
          (fa.keys ++ fb.keys).foldLeft(Map.empty[K, V])((acc, k) =>
            acc.updated(k, fa.getOrElse(k, Monoid[V].zero) |+| fb.getOrElse(k, Monoid[V].zero) )  
          )
          
    
    def sumList[A: Monoid](ls: List[A]): A = foldMap(ls)(identity)
    
    def foldMap[A, B: Monoid](ls: List[A])(f: A => B): B =
      ls.foldLeft(Monoid[B].zero)((acc, e) => acc |+| f(e))
      
    def test2(): Unit = {
      val m1 = Map("a" -> IntAdd(30), "b" -> IntAdd(20))
      val m2 = Map("a" -> IntAdd(30), "d" -> IntAdd(20))
      val r = m1 |+| m2 
      println(s"r is $r")
    }

    def test(): Unit = {
      val r1 = IntMul(3)
      val r2 = IntMul(4)
      val r = r1 |+| r2 
      println(s"r is $r")
      
      val r3 = List(r1, r2)
      val r4 = sumList(r3)
      println(s"r4 is $r4")
      
      val r5 = foldMap(r3)(a => a.toString)
      println(s"r5 is $r5")
      
    }
  @main def semigroup_start(): Unit = {
    Monoid.test()
    Monoid.test2()
  }

}
