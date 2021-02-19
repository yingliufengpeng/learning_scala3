package scala3_function_programming
import scala.reflect.ClassTag


object ch14_local_effects_and_mutable_state {
  
  // Definition of referential transparentcy and purity 
  // An expression e is referenctially transparent if for all programs p all 
  // occurrences of e in p can be repladced by the result of evaluating e 
  // without affecting the meaning of p 
  
  // A function f is pure if the expression f(x) is referentially transparent for 
  // all referentially transparent x.
  
  // A more general definition f referential transparency
  // An expression e is referentially transparent with regard to a progam p if
  // every occurrence of e in p can be replaced by the result of evaluating e 
  // without affecting the meaning of p.
  
  // We say that an effect of e is non-observable by p if it doesn't affect the 
  // referential transparency of e with regard to p. For instance, most programs
  // can't observe the side effect of calling a a constructor, because they don't
  // make use of eq. 
  
  // And referential transparency of e with regard to a program p means that we can rewrite
  // p, replacing every apperance of e with v without changing the meaning of our program.
  
  
  def quicksort(xs: List[Int]): List[Int] =
    if xs.isEmpty then 
      xs
    else
      val arr = xs.toArray
      def swap(x: Int, y: Int): Unit =
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = arr(x)
      
      def partition(n: Int, r: Int, pivot: Int): Int =
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = n 
        for 
          i <- (n until r) 
          if (arr(i) < pivotVal) 
        do 
          swap(i, j)
          j += 1  
        swap(j, r)
        j
      
      def qs(n: Int, r: Int): Unit =
        if n < r then 
          val pi = partition(n, r, n + (n - r) / 2)
          qs(n, pi - 1 )
          qs(pi + 1, r)
       
           
      qs(0, arr.length - 1)
      arr.toList
  
  sealed trait ST[S, A]:
    self =>
    protected def run(s: S): (A, S)
    
    // 这样的执行流从函数的签名的角度来看,
    def withFilter(p: A => Boolean): ST[S, Boolean] = new ST[S, Boolean] {
      override protected def run(s: S): (Boolean, S) =
        val (a, s0) = self.run(s)
//        println(f"kkk is $a  p(a) is ${p(a)}")
        if p(a) then 
          (true, s0)
        else {
          (false, s0)
//          withFilter(v => !p(v)).run(s)
        }
    }
  
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      override protected def run(s: S): (B, S) =
        val (a, s1) = self.run(s)
        (f(a), s1)
    }
  
    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S,B] {
      override protected def run(s: S): (B, S) =
        val (a, s1) = self.run(s)
        f(a).run(s1)
    }
  
  object ST:
    def apply[S, A](a: => A): ST[S, A] =
      lazy val memo = a 
      new ST[S, A] {
        override protected def run(s: S): (A, S) = (memo, s)
      }
  
    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1

  
  trait RunnableST[A]:
    def apply[S]: ST[S, A]
  
  
  sealed trait STRef[S, A]:
    protected var cell: A 
    def read: ST[S, A] = ST(cell)
    
    // 生成新的执行流!!!
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      override protected def run(s: S): (Unit, S) = 
        cell = a 
        ((), s)
    }
  
  // We cannot run a program that tries to return a mutable reference. It's not possible to create
  // a RunnableST that returns a naked STRef.
  // The expression runST(p) uses mutalbe state intenally, but it doesn't have any side effects. As
  // far as any other expression is concerned, it's just a pair of intergers like any other. It will
  // always return the same pair of intergers and it'll do nothing else.
  // In this example, we arbitrarily chose Nothing just to illustrate the point. The point is that 
  // the type S is bound in the apply method, so when we say new RunnableST, that type isn't accessible.
  
  // As a corollary, the fact you can't ge an STRef out of an ST action guarantees that if you have 
  // an STRef, then you are inside of the ST action that created it, so it's always safe to mutate 
  // reference.
  
  // The wildcard type is an artifact of Scala's interoperability with Java's type system. 
  // Fortunately, when you have STRef[_, Int], using it will cause a type error. 
  // This type error is caused by the fact that the wildcard tpe in ref represents some concrete
  // type that only ref knows about. In thiis case it's the S type that we bound in the apply method
  // of the RunnableST where it was created. Scala is unable to prove that this is the same type as 
  // R. Therefore, even though it's possible to abuse the wildcard type to get the naked STRef out, 
  // this is still safe since we can't use it to mutate or access the state.
  //
  
  object STRef:
    // 返回的是ST的类型的实体,可以这样去理解这样的操作
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell: A = a
    })
    
    def test(): Unit = {
      val r = for 
          r1 <- STRef[Nothing, Int](1)
          r2 <- STRef[Nothing, Int](1)
          x <- r1.read 
          y <- r2.read 
          _ <- r1.write(y + 1)
          _ <- r2.write( x + 1)
          a <- r1.read 
          b <- r2.read 
        yield 
          (a, b)
      
      println(f"r is $r")
    }
  
    def test2(): Unit = {
      val p = new RunnableST[(Int,Int)] {
        override def apply[S]: ST[S, (Int, Int)] =
          for
            r1 <- STRef(1)
            r2 <- STRef(1)
            x <- r1.read
            y <- r2.read
            _ <- r1.write(y + 1)
            _ <- r2.write( x + 1)
            a <- r1.read
            b <- r2.read
          yield
             (a, b)
      }
      
      val r = ST.runST(p)
      println(f"r is $r")
    }
  
    def test3(): Unit = {
      val ref = ST.runST(new RunnableST[STRef[_, Int]] {
        override def apply[S]: ST[S, STRef[_, Int]] =  
          for 
            r1 <- STRef(1)
          yield 
            r1 
         
      })
    }
  
  sealed abstract class STArray[S, A]:
    protected def value: Array[A]
  
    def size: ST[S, Int] = ST(value.size)
  
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      override protected def run(s: S): (Unit, S) = {
        value(i) = a 
        ((), s)
      }
    }

    def withFilter(p: A => Boolean): ST[S, STArray[S, A]] = ST.apply(new STArray[S, A] {
      override protected def value: Array[A] = value.filter(p)
    })
  
    def read(i: Int): ST[S, A] = ST(value(i))
  
    def freeze: ST[S, List[A]] = ST(value.toList)
     
    // fill如果使用递归,但是没有运行该执行流,那么就会面临该执行流的数据未能写入到所需的
    // 内存位置之中, 这样返回的结果并非是我们想要的结果!!!
    def fill(xs: Map[Int, A]): ST[S,Unit] =
      xs.foldLeft(ST.apply(()))((acc, e) => {
        val (index, v) = e 
        write(index, v)
      })
      
    
      
    def swap(i: Int, j: Int): ST[S, Unit] =
      for 
        x <- read(i)
        y <- read(j)
        _ <- write(i, y)
        _ <- write(j, x)
      yield 
        ()  

     

  object STArray:
    def noops[S] = ST[S, Unit](())
    
    def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        override protected def value: Array[A] = Array.fill(sz)(v)
      })

    def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value: Array[A] = xs.toArray
      })
      
    // 参考代码
    // https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/localeffects/LocalEffects.scala
    def partition[S](arr: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
      for 
        vp <- arr.read(pivot)
        _ <- arr.swap(pivot, r)
        j <- STRef(l)
        _ <- (l until r).foldLeft(noops[S]) { (acc, i) =>
          for
            _ <- acc // 很好,学会一招,就是把之前需要计算的逻辑给计算一下
            vi <- arr.read(i)
            _ <- if (vi < vp) (
              for
                vj <- j.read
                _ <- arr.swap(i, vj) 
                _ <- j.write(vj + 1)
              yield
                ()
              ) else
                noops[S]
          yield
            ()
          }
              
        x <- j.read 
        _ <- arr.swap(x, r)
      yield 
        x
        
       
    // 这个逻辑自然必须要做重构的过程!!!
    def partition2[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = {
      for 
        pivotVal <- arr.read(pivot)
        _ <- arr.swap(pivot, r)  
      yield 
        var j = n
        for 
          i <- (n until r)
        do  
          for
            e <- arr.read(i)
          yield   
            if e < pivotVal then 
              for 
                _ <- arr.swap(i, j)
              yield 
                j += 1
        for 
          _ <- arr.swap(j, r)
        yield 
          0
        j
    }
 
  
  
    def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
      if r <= n then 
        ST.apply(()) 
      else 
        for
          pi <- partition(a, n, r, n + (r - n) / 2)
          _ <- qs(a, n, pi - 1)
          _ <- qs(a, pi + 1, r)
        yield 
          ()  
  
    def quicksort(xs: List[Int]): List[Int] =
      if xs.isEmpty then 
        xs
      else
        ST.runST(new RunnableST[List[Int]] {
          override def apply[S]: ST[S, List[Int]] = 
            for 
              arr <- STArray.fromList(xs)
              size <- arr.size 
              _   <- qs(arr, 0, size - 1)
              sorted <- arr.freeze
            yield 
              sorted
        })
        
  
    def test(): Unit = {
      val r = quicksort(List(3, 4, 1, 3, 2, 0))
      println(f"r is $r")
      
      val r2 = r.withFilter(_ > 2)
      
     }
      
    
  

  
  @main def local_effects_and_mutable_state_start(): Unit = {
//    STRef.test2()
//    STRef.test3()
    STArray.test()
  }

}
