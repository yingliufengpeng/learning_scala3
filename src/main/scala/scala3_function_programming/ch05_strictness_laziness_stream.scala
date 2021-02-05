package scala3_function_programming

import scala.collection.View.Empty

object ch05_strictness_laziness_stream {

  enum MyStream[+A]:
    case Empty
    
    case Cons(h: () => A, t: () => MyStream[A])
    
     
    def map[B](f: A => B): MyStream[B] =
      foldRight(Empty: MyStream[B])((a, acc) => MyStream.cons(f(a), acc))
    
    
    def map_unfold[B](f: A => B): MyStream[B] =
      MyStream.unfold(this)(s =>  
        s match
          case Empty => None
          case Cons(h, t) => Some( (f(h()) ,t()) )
       
      )
     
    
    def flatMap[B](f: A => MyStream[B]): MyStream[B] =
      foldRight(Empty: MyStream[B])( (a, acc) => f(a).foldRight(acc)((a2, acc) => MyStream.cons(a2, acc)) )
    
    
    def append[A1 >: A](other: => MyStream[A1]): MyStream[A1] =
      foldRight(other)((a, acc) => MyStream.cons(a, acc))
    
    
    def find(p: A => Boolean): Option[A] =
      filter(p).headOption
    
    
    def filter(p: A => Boolean): MyStream[A] =  
      foldRight(Empty: MyStream[A])((a, acc) => if p(a) then MyStream.cons(a, acc) else acc)
  

    def headOption: Option[A] = this match
      case Cons(h, t) => Some(h())
      case _ => None 
//      foldRight((0, None: Option[A]))((a, acc) => if acc._1 == 0 then (1, Some(a)) else acc)._2
//      foldRight(List[A]())((v, acc) => v :: Nil).headOption
 
    //      foldRight((false, false, None: Option[A]))((v, acc) => if !acc._1 && !acc._2 then (false, true, Some(v)) else acc)._3
    
    
    def headOption2: Option[A] = this match
      case Empty => None
      case Cons(h, _) => Some(h())
  
    
    def toList: List[A] = this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    
    
    // f的第二个参数是lazy模式,只需要在计算的时候再去做计算的操作!!!
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z 
    
    
    def startsWith[A1 >: A](prefix: MyStream[A1]): Boolean = (this, prefix) match
      case ( Empty, Empty ) => true 
      case ( Cons(_, _), Empty ) => true
      case ( Cons(h1, t1), Cons(h2, t2)  ) if h1() == h2() => t1().startsWith(t2())
      case _ => false
       
    
    def tails: MyStream[MyStream[A]] =
      MyStream.unfold(this)(s =>
        s match
          case Empty => None
          case p@Cons(_, t) => Some( (p, t()) )
      )
    
    
    def hasSubsequence[A1 >: A](s: MyStream[A1]): Boolean =
      tails.exists( _ startsWith s )
    
    
    def scanRight[B](z: => B)(f: (A, => B) => B): MyStream[B] =
      foldRight((MyStream.cons[B])(z, Empty))((a, acc) => 
        acc.headOption match
          case Some(v) => MyStream.cons(f(a, v), acc)
          case None => Empty
      )
    
    
    def scanRight2[B](z: => B)(f: (A, => B) => B): MyStream[B] =  
      MyStream.unfold(this)(s => 
        s match
          case Empty => None
          case p@Cons(h, t) => Some(f(h(), z), t())  
         
      )
     
       
    
    
    def zipAll[B](s2: MyStream[B]): MyStream[(Option[A], Option[B])] =
      MyStream.unfold((this, s2))(s =>
        s match
          case (Empty, Cons(h2, t2)) => Some( (None, Some(h2())), (Empty, t2())   )  
          case (Cons(h1, t1), Empty  ) => Some( ((Some(h1()), None ), (t1(), Empty) ) )
          case ( Cons(h1, t1), Cons(h2, t2) ) => Some( ( ( Some(h1()), Some(h2())), (t1(), t2()) ) )
          case _ => None
      )
    
    
    def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    
    
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((e, acc) => p(e) && acc)
     
    
    def exists(p: A => Boolean): Boolean =
      foldRight(false)((e, acc) => p(e) || acc)
    
    
    def exists2(p: A => Boolean): Boolean = this match
      case Cons(h, t) => p(h()) ||  t().exists(p)
      case _ => false
 
    
    def takeWhile3(p: A => Boolean): MyStream[A] =  
      foldRight((true, Empty: MyStream[A]))((v, acc) =>  if p(v) then (acc._1, MyStream.cons(v, acc._2)) else acc )._2
    
    def takeWhile(p: A => Boolean): MyStream[A] =
      MyStream.unfold(this)(s =>
        s match
          case Cons(h, t) if (p(h())) => Some(h(), t())
          case _ => None 
      )  

    def takeWhile2(p: A => Boolean): MyStream[A] = 
      def go(remaining: MyStream[A], acc: MyStream[A]): MyStream[A] =
        remaining match
          case Empty => acc
          case Cons(h, t) =>
            lazy val head = h() 
            if p(head) then  
              MyStream.cons(head, go(t(), acc) )
            else
              acc 

      go(this, Empty)
  
    def take_unfold(n: Int): List[A] =
      MyStream.unfold((1, this))(s =>
        s match
          case (index, Cons(h, t)) if index <= n && index > 0 => Some(h() ,(index + 1, t()))
          case _ => None
      ).toList
    
    def take(n: Int): List[A] =  
      def go(index: Int, lastRemaining: MyStream[A], acc: List[A] = Nil): List[A] =
        if index > n then 
          acc
        else
          lastRemaining  match
            case Empty => Nil
            case Cons(h, t) => go(index + 1, t(), acc :+ h())

      go(1, this)
     
    
    def drop(n: Int): MyStream[A] =
      def go(index: Int, lastRemaining: MyStream[A]): MyStream[A] =
        if index > n then 
          lastRemaining
        else
          lastRemaining match
            case Empty => Empty
            case Cons(_, t) => go(index + 1, t())
            
      go(1, this)
  
  
  object MyStream:
    def apply[A](as: A*): MyStream[A] =
      if as.isEmpty then 
        Empty
      else
        cons(as.head, apply(as.tail: _*))
 
    
    def cons[A](hd: => A, t1: => MyStream[A]): MyStream[A] =
      lazy val head = hd 
      lazy val tail = t1
      Cons(() => head, () => tail)
//      Cons(() => hd, () => t1)
  
    
    def constant[A](a: A): MyStream[A] =  
      lazy val r: MyStream[A] = cons(a, r)
      r 
 
    
    def from_func_tuple2[A](first: A, second: A)(f: (A, A) => A): MyStream[A] =
      cons(first, from_func_tuple2(second, f(first, second))(f))

    
    def fibs: MyStream[BigInt] =
      from_func_tuple2[BigInt](0, 1)((x, y) => x + y)
      
    
    def from_func[A](start: A)(f: A => A): MyStream[A] =
      cons(start, from_func(f(start))(f))
    
    
    // 这种递归的模型,关键还是在于lazy的特性 => Mystream[Int]的类型惰性特质
    def from(start: Int, step: Int): MyStream[Int] =
      cons(start, from(start + step))
    
    
    def from(n: Int): MyStream[Int] =
      from(n, 1)


    def unfold[A, S](s0: S)(f: S => Option[(A, S)]): MyStream[A] =  
      f(s0) match
        case Some(v, s) => cons(v, unfold(s)(f))
        case None => Empty
        
    
    def from_unfold(n: Int): MyStream[Int] =
      unfold(n)(s => Some(s, s + 1))
      
    
    def fibs_unfold: MyStream[BigInt] =
      unfold((0, 0, 1))(s => {
          val index = s._1
          val first = s._2 
          val second = s._3 
          val next_value = first + second
          if index == 0 || index == 1 then 
            Some(index, (index + 1, 0, 1))
          else
            Some(next_value, (index + 1, second, next_value))
      })
      
   
      
    
    def empty[A]: MyStream[A] = Empty
  

  @main def strict_laziness_stream_start(): Unit = {
    
    val r = MyStream(1, 2, 3, 4, 5, 6) 
    println(f"r is ${r.toList}")
    
    val rr = MyStream(1, 2, 3, 4, 5, 6, 7) 
    println(f"rr is ${rr.toList}")
    
    val r2 = r.toList
    println(f"r2 is $r2")
    
    val r3 = r.take(3).toList
    println(f"r3 is $r3")
    
    val r4 = r.drop(4).toList
    println(f"r4 is $r4")
    
    val r5 = r.takeWhile(e => e < 5).toList
    println(f"r5'takeWhile is $r5")
    
    val r6 = r.exists(e => e == 44)
    println(f"r6 is $r6")
    
    val r7 = r.headOption
    println(f"r7 is $r7")
    
    
    val r8 = r.map(_ * 2).toList
    println(f"r8 is $r8")
    
    val r9 = r.flatMap(e => MyStream(e, e, e, e, e )).toList
    println(f"r9 is $r9")
    
    val r10 = r.append(r).toList.mkString(", ")
    println(f"r10 is $r10")
    
    val r11 = r.filter( _ % 2 == 0).toList
    println(f"r11 is $r11")
    val r12 = r.find( _ % 2 == 0).toList
    println(f"r12 is $r12")
    
    lazy val r13: MyStream[Int] = MyStream.cons(1, r13)
    
    val r14 = r13.take(4).toList
    println(f"r14 is $r14")
    
    val r15 = r13.headOption
    println(f"r15 is $r15")
    
    
    val r16 = MyStream.constant(4).take(10).toList
    println(f"r16 is $r16")
    
    val r17 = MyStream.from(1).take(10).toList
    println(f"r17 is $r17")
    
    val r18 = MyStream.from_func(BigInt(1))(_ * 2).take(10).toList.mkString(", ")
    println(f"r18 is $r18")
    
    val r19 = MyStream.fibs.take(10).toList
    println(f"r19 is $r19")
    
    val r199 = MyStream.fibs_unfold.take(10).toList
    println(f"r199 is $r199")
    
    val r20 = MyStream.from_unfold(1).take(10).toList
    println(f"r20 is $r20")
    
    val r30 = r.map_unfold(_ * 2).toList
    println(f"r30 is $r30")
    
    
    val r31 = r.take_unfold(3)
    println(f"r31 is $r31")
    
    val r32 = r.takeWhile( _ < 10).toList
    println(f"r32 is $r32")
    
    val r33 = r.zipAll(rr).toList
    println(f"r33 is $r33")
    
    val r34 = rr.startsWith(rr)
    println(f"r34 is $r34")
    
    val r35 = r.tails.toList.map(_.toList)
    println(f"r35 is $r35")
    
    val r36 = rr.hasSubsequence(r)
    println(f"r36 is $r36")
    
    val r37 = MyStream(1, 2, 3).scanRight(0)(_ + _).toList
    println(f"r37 is $r37")
    
  }
}
