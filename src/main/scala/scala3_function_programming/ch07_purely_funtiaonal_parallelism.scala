package scala3_function_programming
import concurrent.{Await, ExecutionContext, Future}
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration._
import scala.annotation.targetName

object ch07_purely_funtiaonal_parallelism {
  
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((acc, v) => acc + v)
    
  
  def sum2(ints: IndexedSeq[Int]): Int =
    if ints.size <= 1 then
      ints.headOption.getOrElse(0)
    else
      val (l, r) = ints.splitAt(ints.length / 2)
      sum2(l) + sum2(r)
      
  
  object Parallel:
//    import concurrent.{Await, ExecutionContext, Future}
//    import concurrent.ExecutionContext.Implicits.global
//    given ExecutionContext = global
//    import concurrent.duration._
    type Par = [A] =>> ExecutionContext ?=> Future[A] // 我们这里使用scala3的最新的上下文绑定的写法
  
    
    def unit[A](a: A): Par[A] = Future(a)
    def delay[A](fa: => Par[A]): Par[A] = fa
    def fork[A](a: => Par[A]): Par[A] = Future(a).flatten
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => 
        lazyUnit(f(a))
        
    
    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      val r = ps.map(asyncF(f))
      seqence(r)
    
    
    def parFilter[A](ps: List[A])(p: A => Boolean): Par[List[A]] =
//      val r2 = ps.map(asyncF(identity)).flatMap(par => ( par map2 lazyUnit(asyncF(p)) ) ((v, pf) => if pf(v) then List(v) else Nil) )
      val r = ps.map(asyncF(identity)).filter(par => Await.result(par.map(p), Duration.Inf))
      seqence(r)
        
    
    def seqence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldLeft(unit(List.empty[A]))((acc, a) => 
        for 
          res <- acc 
          y <- a 
        yield 
          y :: res
      )
   
     
    def inner_sum(ints: IndexedSeq[Int]): Par[Int] =
      if ints.size <= 1 then 
        unit(ints.headOption.getOrElse(0))
      else
        val (l, r) = ints.splitAt(ints.length / 2)
        (fork(inner_sum(l)) map2 fork(inner_sum(r)))(_ + _)
        
    
    
    def sum(ints: IndexedSeq[Int]): Int =
      Await.result(inner_sum(ints), Duration.Inf)
      
    
    def fold[A](ints: IndexedSeq[A])(init: A)(f: (A, A) => A): Par[A] = {
      if ints.size == 1 then 
        unit(f(init, ints.head))
      else if ints.size < 1 then 
        unit(init)
      else
        val (l, r) = ints.splitAt(ints.length / 2)
        (fork(fold(l)(init)(f)) map2 fork(fold(r)(init)(f)))(f)
    }
  
       
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
      val r = cond.map(c => if c then 0 else 1)
      choiceN(r)(List(t, f))
  
    
    def choiceN2[A](parn: Par[Int])(choices: List[Par[A]]): Par[A] =
      parn.flatMap(index => choices(index) )
      
    
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      key.flatMap(k => choices(k))
    
    // chooser == flatMap
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      pa.flatMap(e => choices(e))
      
    //  // 用 flatMap 来实现 flatten
    @targetName("join")
    def flatten[A](par: Par[Par[A]]): Par[A] =
      par.flatMap(e => e)
      
    
    def choiceN[A](par_n: Par[Int])(choices: List[Par[A]]): Par[A] = 
//      chooser(par_n)(choices)
      chooser(par_n)(n => choices(n))
      
  
    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      parList.map(_.sorted)
      
    
    def max(ints: IndexedSeq[Int]) =
      fold(ints)(Int.MinValue)(math.max)
      
    
    def min(ints: IndexedSeq[Int]) =
      fold(ints)(Int.MaxValue)(math.min)
      
    
    def equal[A](pa: Par[A], pb: Par[A]): Par[Boolean] =
      (pa map2 pb)(_ == _) 
      
    
    def test(): Unit = {
      val r = unit(1).map(_ + 1) == unit(2)
      println(f"r is $r")
      
      
      val r1 = unit(3)
      val r2: List[Par[Int]] = (1 to 10).toList.map(lazyUnit)
      
      val r3 = choiceN[Int](r1)(r2)
      val r33 = Await.result(r3, Duration.Inf)
      println(f"r33 is $r33")
      
      val r4: Par[Par[Int]] = unit(r1)
      val r5 = flatten(r4)
      val r6 = Await.result(r5, Duration.Inf)
      println(f"r6 is $r6")
      
    }
   
    
    
    extension [A, B, C](par: Par[A]) 
      def run: A = par.value.get.get
      def get: A = Await.result(par, Duration.Inf)
      def map(f: A => B): Par[B] = flatMap(e => Parallel.lazyUnit(f(e))) 
      def flatMap(f: A => Par[B]): Par[B] = Parallel.fork(f(par.run))
      def map2(parb: Par[B])(f: (A, B) => C): Par[C] =
        for 
          i <- par 
          j <- parb
        yield 
          f(i , j)  
      
  
  @main def purley_functional_start(): Unit = {
    val r0 = 1 to 100
    val r = sum(r0)
    val r2 = sum2(r0)
    println(f"r is $r, r2 is $r2")
    
    val r3 = Parallel.sum(1 to 100)
    println(f"r3 is $r3")

    val r5 = sum(r0)
    println(f"r5 is $r5")

    val r4 = Parallel.sum(r0) 
    println(f"r4 is $r4")
    
    
    val r6 = Parallel.parMap[Int, BigInt](r0.toList)(_ * 2)
    val r7 = Await.result(r6, Duration.Inf).sum
    println(f"r7 is $r7")

    val r8 = Parallel.parFilter(r0.toList)(_ % 2 == 0)
    val r9 = Await.result(r8, Duration.Inf) 
    println(f"r9 is $r9")
    
    val r10 = Parallel.max(r0)
    val r11 = Await.result(r10, Duration.Inf)
    println(f"r11 is $r11")


    val r12 = Parallel.min(r0)
    val r13 = Await.result(r12, Duration.Inf)
    println(f"r11 is $r13")


    Parallel.test()






  }

}
