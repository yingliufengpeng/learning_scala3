package scala3_function_programming
 
object ch03_function_data_structures {
  
  enum MyList[+A]:
    case Cons(head: A, tail: MyList[A])
    case Nil

    override def toString: String = this match
      case Nil => ""
      case Cons(h, t) => s"$h,$t"
  
  
  object MyList:
    def sum2(ints: MyList[Int]): Int = ints match
      case Nil => 0
      case Cons(x, xs) => x + sum2(xs)
    
    
    def sum(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)
    
  
    def product2(ds: MyList[Double]): Double = ds match
      case Nil => 1.0
      case Cons(0.0, _) => 0.0 // 优化写法
      case Cons(h, t) => h * product2(t)
    
    
    def product(ds: MyList[Double]): Double = foldRight(ds, 1.0)(_ * _)
    
    
    def length[A](l: MyList[A]): Int = foldLeft(l, 0)((_, acc) => acc + 1)
    
    
    def map2[A, B](l: MyList[A])(f: A => B): MyList[B] = l match
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map2(t)(f))
    
    
    def map[A, B](l: MyList[A])(f: A => B): MyList[B] =
      foldRight(l, Nil: MyList[B])((a, acc) => Cons(f(a), acc))
    
    
    def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
      foldRight(l, Nil: MyList[B])((a, acc) => append2(f(a), acc))
  
    
    def filter2[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter2(t)(f))
      case Cons(_, t) => filter2(t)(f) 
    
    
    def filter[A](l: MyList[A])(f: A => Boolean): MyList[A] =
      flatMap(l)(e => if f(e) then Cons(e, Nil) else Nil)
  
    
    def apply[A](as: A*): MyList[A] =
      if as.isEmpty then
        Nil
      else
        Cons(as.head, apply(as.tail *))  
        
    
    def tail[A](as: MyList[A]): MyList[A] = as match
      case Nil => Nil
      case Cons(_, t) => t 
    
    
    def revese[A](l: MyList[A]): MyList[A] = foldLeft(l, Nil: MyList[A])(Cons(_, _))
    
    
    def add[A](as: MyList[A], v: A): MyList[A] =
      Cons(v, as)
      
    
    def setHead[A](as: MyList[A], v: A): MyList[A] = as match
      case Nil => Cons(v, Nil)
      case Cons(h, t) if t != v => Cons(v, t)
      case p => p 
    
    
    def drop[A](l: MyList[A], n: Int): MyList[A] = 
      
      def go(index: Int = 0,  last_remaining: MyList[A] = l): MyList[A] =
        if index >= n || (last_remaining == Nil) then
          last_remaining
        else
          go(index + 1, tail(last_remaining))
  
      go()
    
    
    def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] = l match
      case Nil => Nil
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case t => t   
    
    
    def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    
    
    def append2[A](a1: MyList[A], a2: MyList[A]): MyList[A] = 
      foldRight(a1, a2)((v, acc) => Cons(v, acc))
    
    
    def concat[A](as: MyList[A]*): MyList[A] = as match
      case Nil => Nil
      case Seq(f) => f
      case Seq(f, s, t:_*) => concat( (append2(f, s) :: t.toList) * )
    
    def concat2[A](as: MyList[MyList[A]]): MyList[A] =
      foldRight(as, Nil: MyList[A])((list, acc) => append2(list, acc))
      
    
    def init[A](l: MyList[A]): MyList[A] = l match
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    
    
    def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = l match
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t,z)(f))
    
    
    def foldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = l match
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, z))(f)
    
    
    // Hard!!!  write foldLeft in terms of foldRight  // 延迟计算 最后再一起计算
    def foldLeft2[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = 
      foldRight(l, identity[B])((v, acc_fun) => (b: B) => acc_fun(f(v, b)) )(z)  
    
    
    // write foldRight in terms of foldLeft 
    def foldRight2[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =  
      foldLeft(l, identity[B])((v, acc_fun) => (b: B) => acc_fun(f(v, b)) )(z)
       
    
    def test(): Unit = {
      val x = MyList(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => println("ik"); x 
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => println("kk"); x + y // 这样的写法更为具体一些
        case Cons(h, t) => h + sum(t)
        case Nil => 42
      }
      println(f"x is $x")
    }
    
    def zipWith[A, B, C](first: MyList[A], second: MyList[B])(f: (A, B) => C): MyList[C] =
      (first, second) match
        case (Nil, t) => Nil
        case (t, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2) ) => Cons(f(h1, h2), zipWith(t1, t2)(f))
        
    
    def forAll[A](l: MyList[A])(f: A => Boolean): Boolean =
      foldLeft(l, true)((a, acc) => f(a) && acc)
      
    
    def exists[A](l: MyList[A])(f: A => Boolean): Boolean =
      foldLeft(l, false)((a, acc) => f(a) || acc)
        
    
    def add2MyList(first: MyList[Int], second: MyList[Int]): MyList[Int] =
      zipWith(first, second)((x, y) => x + y)
      
    // implement hasSubsequence for checking whether a  List contains
    // another List as a subsequence
    def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean =  
      def go(matched: MyList[A], baseremaing: MyList[A], subReamining: MyList[A]): Boolean =
        (baseremaing, subReamining) match
          case (Nil, Nil) => true
          case (Nil, _) => false
          case (_, Nil) => true
          case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => go(Cons(h1, matched), t1, t2)
          case (Cons(h1, t), Cons(h2, _)) if h1 != h2 => go(Nil, t, sub)
          case _ => false
         
     
      go(Nil, sup, sub)
   
  
  @main def function_data_structures_start(): Unit = {
    val r = MyList(3, 4, 5, 6, 7)
    println(f"r is $r")
    
    MyList.test()
    
    val r2 = MyList.drop(r, 45)
    println(f"r2 is $r2")
    
    val r3 = MyList.dropWhile(r, n => n <= 4)
    println(f"r3 is $r3")
    
    val r4 = MyList.append(r, r)
    println(f"append r4 is $r4")
    
    val r5 = MyList.init(r)
    println(f"r5 is $r5")
    
    val r6 = MyList.foldLeft(r, MyList.Nil: MyList[Int])((x, y) => MyList.Cons(x, y))
    println(f"foldLeft r6 is $r6")

    val r7 = MyList.foldRight(r, MyList.Nil: MyList[Int])((x, y) => MyList.Cons(x, y))
    println(f"foldRight r7 is $r7")
    
    val r8 = MyList.sum(r)
    println(f"r8 is $r8")
    
    val r9 = MyList.product(MyList.map(r)(_.toDouble))
    println(f"r9 is $r9")
    
    val r10 = MyList.length(r)
    println(f"r10 is $r10")
    
    val r11 = MyList.revese(r)
    println(f"r11 is $r11")
    
    val r12 = MyList.foldLeft2(r, MyList.Nil: MyList[Int])((x, y) => MyList.Cons(x, y))
    println(f"foldLeft2 r12 is $r12")
    
    val r13 = MyList.foldRight2(r, MyList.Nil: MyList[Int])((x, y) => MyList.Cons(x, y))
    println(f"foldRight2 r13 is $r13")
    
    val r14 = MyList.append2(r, r)
    println(f"append2 r14 is $r14")
    
    val r15 = MyList.concat(r, r, r, r, r, r, r, r, r, r)
    println(f"r15 is $r15")
    
    val r16 = MyList.map(r)(_.toString + "333")
    println(f"map r16 is $r16")
    
    val r17 = MyList.filter(r)(_ % 2 == 0)
    println(f"filter r17 is $r17")
    
    val r18 = MyList.flatMap(r)(e => MyList(e, e))
    
    println(f"r18 is $r18")
    
    val r19 = 3 + 4 
    val r20 = 3
    val r21 = 4
    val r22 = r20 + r21 
    
    val r23 = MyList.add2MyList(r, r)
    println(f"r23 is $r23")
    
    val r24 = MyList.forAll(r)(_ > 2)
    println(f"r24 is $r24")

    val r25 = MyList.exists(r)(_ < 5)
    println(f"r25 is $r25")
    
    val r26 = MyList.hasSubsequence(r, MyList(5))
    printf(f"r26 is $r26")
    
    
  }

}
