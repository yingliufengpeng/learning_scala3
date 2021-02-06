package scala3_function_programming

object ch06_purely_better_api_for_state_actioins {
  
  trait RNG:
    def nextInt: (Int, RNG)
    
  
  type Rand[+A] = RNG => (A, RNG) // Function
  
   
  case class SimpleRNG(seed: Long) extends RNG:
    override def nextInt: (Int, RNG)  =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  
   
  extension [A, B] (f_rand: Rand[A])
    def map2(f: A => B): Rand[B] = 
      rng => 
        val (r1, rng1) = f_rand(rng)
        (f(r1), rng1)
    
    
    def flatMap(g: A => Rand[B]): Rand[B] =
      rng =>
        val (r, rng0) = f_rand(rng)
        g(r)(rng0)
    
    
    def map(f: A => B): Rand[B] =  
      flatMap{ e => 
        rng => 
          (f(e), rng)
      }
   

    def take(n: Int): Rand[List[A]] =
      rng =>
        if n < 1 then 
          (Nil, rng)
        else
          val (r1, rng1) = f_rand(rng)
          lazy val res = take(n - 1)(rng1)
          (r1 :: res._1, res._2)
           
  
  object Rand:
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    
    
    // 最为基本初始化的写法
    def int: Rand[Int] = _.nextInt
  
    
    def double: Rand[Double] = int.map(_.toDouble)
  
    
    def nonNegativeInt: Rand[Int] = int.map(e => math.abs(e))
  
    
    def nonNegativeEven: Rand[Int] = nonNegativeInt.map(e => e - e % 2)
  
    
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng =>
        val (resa, rng1) = ra(rng)
        val (resb, rng2) = rb(rng1)
        (f(resa, resb), rng2)
  
    
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))
      
    
    def randIntDouble: Rand[(Int, Double)] = both(int, double)
    
  
    def randDoubleInt: Rand[(Double, Int)] = both(double, int)
    
    // List.fill(1024){ val x = 3; x + 1 } // 后面的代码则是懒加载的写法!!!
  
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      rng =>
        fs.foldLeft((List.empty[A], rng)) { case ((vs, rng0), rand_func) =>
          val (r, rng1) = rand_func(rng0)
          (r :: vs, rng1)
        }
    }

    //
    //
    def ints[A](n: Int)(rand: Rand[A]): Rand[List[A]] =
      rng =>
        val r = List.fill(n)(rand)
        sequence(r)(rng)
    
    // This will certainslly generate a number in the range, but it'wlll be skewed because Int.MaxValue may 
    // not be exactly by n.
    def nonNegativeLessThan2(n: Int): Rand[Int] =
      rng =>
        
        val (i, rng1) = nonNegativeInt(rng)
        val mod = i % n 
        if (i + (n - 1) - mod >= 0)
          (mod, rng1)
        else
          nonNegativeLessThan2(n)(rng)
        
    def nonNegativeLessThan(n: Int): Rand[Int] =
      nonNegativeInt.flatMap{ e => 
        val mod = e % n 
        rng =>
          if (mod + (n - 1) - mod >= 0)
            (mod, rng)
          else
            nonNegativeLessThan2(n)(rng)
      }
       
  
    def rollDie: Rand[Int] = nonNegativeLessThan(6)
    
         

  @main def better_api_for_start(): Unit = {

    val rng = SimpleRNG(42)
    
    val r0 = Rand.int 
     
    val r = Rand.int(rng)
    println(f"r is $r")
    
    val r2 = Rand.double(rng) 
    println(f"r2 is $r2")
    
    val r3 = Rand.int.take(10)(rng)
    println(f"r3 is $r3")
    
    val r4 = Rand.double.take(10)(rng)
    println(f"r4 is $r4")
    
    val r5 = Rand.unit(10).take(10)(rng)
    println(f"r5 is $r5")
    
    val r6 = Rand.nonNegativeEven.take(10)(rng)
    println(f"r6 is $r6")
    
    val r7 = Rand.map2(Rand.int, Rand.double)((i, d) => f"$i  $d").take(5)(rng)
    println(f"r7 is $r7")
    
    val r8 = Rand.randDoubleInt.take(3)(rng)
    println(f"r8 is $r8")
   
    val r9 = Rand.sequence(List(r0, r0, r0)).take(2)(rng)
    println(f"r9 is $r9")
    
    val r10 = Rand.ints(10)(Rand.int)(rng)
    println(f"r10 is $r10")
    
    
    val r11 = Rand.int.flatMap(e => Rand.int).take(2)(rng)
    println(f"r11 is $r11")
    
    
    val r12 = Rand.nonNegativeLessThan(10).take(10)(rng)
    println(f"r12 is $r12")
    
    val r13 = Rand.nonNegativeLessThan(10).map(_ * 2).take(10)(rng)
    println(f"r13 is $r13")
    
    val r14 = Rand.rollDie.take(2)(rng)
    println(f"r14 is $r14")
    
    
 
    
    
    
    
  }

}
