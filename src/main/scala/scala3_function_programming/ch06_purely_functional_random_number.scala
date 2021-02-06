package scala3_function_programming

object ch06_purely_functional_random_number {
  
  trait RNG:
    def nextInt: (Int, RNG) 
  
  
  case class SimpleRNG(seed: Long) extends RNG:
    override def nextInt: (Int, RNG)  =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt 
      (n, nextRNG)
  
  
  def radomPair(rng: RNG): ((Int, Int), RNG) =
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  
  
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (r, rng1) = rng.nextInt
    if r < 0 then 
      nonNegativeInt(rng1)
    else if r == Int.MinValue then 
      nonNegativeInt(rng1)
    else
      (r, rng1)
      
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then 
      (Nil, rng)
    else 
      val (r, rng1) = rng.nextInt
      lazy val res = ints(count - 1)(rng1)
      (r :: res._1, res._2)   
  
  def take[A](rng: RNG, n: Int)(f: Int => A): List[A] =
    if n <= 0 then 
      Nil
    else 
      val (r1, rng1) = nonNegativeInt(rng)
      f(r1) :: take(rng1, n - 1)(f)
      
  
  def double(rng: RNG): (Double, RNG) = 
    val (r, rng1) = nonNegativeInt(rng)
    if r == Int.MaxValue then 
      double(rng1)
    else
      (r.toDouble / (Int.MaxValue).toDouble, rng1)
      
  def take_doulbe(rng: RNG, n: Int): List[Double] =
    if n < 0 then 
      Nil
    else 
      val (r, rng1) = double(rng)
      r :: take_doulbe(rng1, n - 1)
    
  
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
    
    

  @main def purely_functional_random_start(): Unit = {
    
    val rng = SimpleRNG(42)
    val (n1, rng1) = rng.nextInt
    println(f"n1 is $n1")
    
    val (n2, rng2) = rng1.nextInt
    println(f"n2 is $n2")
    
    val r3 = take(rng, 10)(_)
    println(f"r3 is $r3")
    
    val r4 = take_doulbe(rng, 20)
    println(f"r4 is $r4")
     
    val r5 = ints(3)(rng)
    println(f"r5 is $r5")
  }

}
