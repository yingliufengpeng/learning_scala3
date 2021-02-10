package scala3_function_programming

import scala3_function_programming.ch08_property_based_testing.{Gen, Prop}

object ch10_monoids {
  
  trait Monoid[A]:
    self =>
    
    def zero: A  
    def op(a1: A, a2: A): A  
    
    extension (a1: A) 
      def bin(a2: A): A = self.op(a1, a2)
  
  trait Foldable[F[_]]:
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B 
    
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldRight(as)(identity[B]) { case (a, acc) =>
        b =>
          acc(f(b, a))
      }(z)
      
    
    def foldMap[A, B: Monoid](as: F[A])(f: A => B): B =  
      val m = summon[Monoid[B]]
      foldLeft(as)(m.zero)((acc, a) => m.op(f(a), acc))
     


    def concatenate[A: Monoid](as: F[A]): A = 
      val m = summon[Monoid[A]]
      foldLeft(as)(m.zero)(m.op)
  
    def toList[A](fa: F[A]): List[A] =
      foldLeft(fa)(List.empty)((acc, a) => a :: acc)
      
    
  
  enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])
  
    def foldRight[B](z: B)(f: (A, B) => B): B = this match
      case Leaf(v) => f(v, z)
      case Branch(left, right) => left.foldRight(right.foldRight(z)(f))(f)
  
  
  
  object Foldable:
    given Foldable[List] = new Foldable[List]:
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    
    given Foldable[IndexedSeq] = new Foldable[IndexedSeq]:
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    
    given Foldable[Stream] = new Foldable[Stream]:
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    
    given Foldable[Tree] = new Foldable[Tree]:
      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    
    
    given Foldable[Option] = new Foldable[Option]:
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

   


  object Monoid:
    
    enum WC:
      case Stub(chars: String)
      case Part(lStub: String, words: Int, rStub: String)
    
    object WC:
      def findWords(str: String): WC =
        str.split(' ').filterNot(e => e.contains(" ")).toList match
          case Nil => Stub(str)
          case p => 
            val lindex = str.indexOf(p.head)
            val rindex = str.lastIndexOf(p.last)
            Part(str.substring(0, lindex), p.length, str.substring(rindex))
    
    import WC._ 
    
    
    given Monoid[WC] = new Monoid[WC] {
      override def zero: WC = Part("", 0, "")
      override def op(a1: WC, a2: WC): WC = (a1, a2) match
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => 
          findWords(l1 + r1 + l2 + r2) match
            case Stub(ps) => Part(l1 + l2, w1 + w1, r1 + r2)
            case Part(l, w0, r) => Part(l, w1 + w2 + w0, r)
        case (Stub(cs), Part(l, w, r)) => Part(cs + l, w, r)
        case (Part(l, w, r), Stub(cs)) => Part(l, w, r + cs )
        case (Stub(cs1), Stub(cs2)) => Stub(cs1 + cs2)
    }
    
    given Monoid[String] = new Monoid[String] {
      override def zero: String = ""
      override def op(a1: String, a2: String): String = a1 + a2
    }

    
    given addMonid: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def op(a1: Int, a2: Int): Int = a1 + a2
    }

    
    given intMulMonid: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 1
      override def op(a1: Int, a2: Int): Int = a1 * a2
    }

    
    given boolOrMonid: Monoid[Boolean] = new Monoid[Boolean] {
      override def zero: Boolean = false
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    }
    
    
    given boolAndMonid: Monoid[Boolean] = new Monoid[Boolean] {
      override def zero: Boolean = true
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }
    
 
    
    given intsorderMonoid: Monoid[IndexedSeq[Int]] = new Monoid[IndexedSeq[Int]] {
      override def zero: IndexedSeq[Int] = IndexedSeq.empty // 为空说明是排序成功的
      override def op(a1: IndexedSeq[Int], a2: IndexedSeq[Int]): IndexedSeq[Int] =
        if a1.sorted == a1 && a2.sorted == a2 then
          IndexedSeq.empty
        else
          a1 ++ a2
    }
    
    def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map.empty[K, V]
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, mv.op(a1.getOrElse(k, mv.zero), a2.getOrElse(k, mv.zero)  ))
        }
    }
    
    def stringCount(input: String): WC =
      val r = summon[Monoid[WC]] 
      if input == "" then 
        r.zero
      else 
        val (le, ri) = input.splitAt(input.length / 2)
        r.op( stringCount(le), stringCount(ri) )

    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      override def zero: (A, B) = (a.zero, b.zero)

      override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    }
    
    
    def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
      override def zero: Option[A] = None
      override def op(a1: Option[A], a2: Option[A]): Option[A] = {
        val v = summon[Monoid[A]]
        (a1, a2) match
          case (Some(a), Some(b)) => Some(v.op(a, b))
          case (p, None) => p
          case (None, p) => p
          case None => None
      }

    }
    
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      override def zero: A => B = a => B.zero

      override def op(a1: A => B, a2: A => B): A => B =
        a =>
          B.op(a1(a), a2(a))
    }
    
    def bag[A: Monoid](as: IndexedSeq[A]): Map[A, Int] =  
      val m = addMonid
      as.foldLeft(Map.empty[A, Int])((acc, k) => acc.updated(k, m.op(1, acc.getOrElse(k, m.zero))))
     


    def listMonoid[A] = new Monoid[List[A]] {
      override def zero: List[A] = Nil
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    }
    
    
    def concatenate[A: Monoid](as: List[A]): A =
      val m = summon[Monoid[A]]
      as.foldLeft(m.zero)(m.op)
    
    
    // 应该是foldLeft,但是如果那样实现,就会递归溢出
    def foldRight[A, B: Monoid](as: List[A])(zero: B)(f: (A, B) => B): B =
      foldMap(as)(a => f(a, zero))
    
    
    def foldMap[A, B: Monoid](as: List[A])(f: A => B): B =
      val m_b = summon[Monoid[B]]
      as.map(f).foldLeft(m_b.zero)(m_b.op)
    
    
    def is_ordered(as: IndexedSeq[Int]): IndexedSeq[Int] = 
//      val r = summon[Monoid[IndexedSeq[Int]]]
      foldMap(List(as))(e => e)
      
    

    def foldMapV[A, B: Monoid](v: IndexedSeq[A])(f: A => B): B =  
      val m_b = summon[Monoid[B]]
      if v.length < 1 then 
        m_b.zero
      if v.length == 1 then
        f(v.head)
      else
        val (le,ri) = v.splitAt(v.length / 2)
        m_b.op(foldMapV(le)(f), foldMapV(ri)(f))

    def test(): Unit = {
      val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(addMonid))
      
      val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
      val m2 = Map("o1" -> Map("i2" -> 3))
      val m3 = M.op(m1, m2)
      println(f"m3 is $m3")
      
      val m4: Monoid[(Int, Int)] = productMonoid(addMonid, addMonid)
      println(f"m4 is $m4")
      val m5 = summon[Foldable[List]]
      val p = m5.foldMap(List(1, 2, 3, 4))(a => (1, a))(using m4)
      println(f"p is $p")
      
    }

    object monoidLaw:
      def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
        gen.forAll { v =>
          m.op(m.zero, v) == v == m.op(v, m.zero)
        }
      
      
    
    

  end Monoid
 
  
  
  @main def monoids_start(): Unit = {
    Monoid.test()
  }

}
