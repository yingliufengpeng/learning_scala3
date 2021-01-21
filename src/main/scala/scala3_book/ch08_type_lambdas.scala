package scala3_book

object ch08_type_lambdas {

  type K = [X, Y] =>> Map[X, Y]
  
  // type TL1 = [X >: L1 <: U1] =>> R1 
  // type TL2 = [X >: L2 <: U2] =>> R2 
  
  // Then TL1 <: TL2 if 
  // the type interval L2..U2 is contained in the type interval L1..U1 (i.e. L1 <: L2 and U2 <: U1) 
  // R1 <: R2 
  
  type T[X] = Int // is regaded ad a shorthand for an unparameterized definition with type 
  // lambda as right-hand side 
  // type T = [X] =>> Int
  
  type F2[A, +B] = A => B 
  type F22 = [A, B] =>> A => B
  
  // A parameterized abstact type is regarded as shorthand for an unparameterized abstract type 
  // with type lambdas as bounds 
  type T2[X] >: Nothing <: Any 
  type T3 >: ([X] =>> Nothing) <: ([X] =>> Any)

  type T4[X] <: X => X 
  type T44 >: Nothing <: ([X] =>> X => X) 
  // instead of 
  type T444 >: ([X] =>> Nothing) <: ([X] =>> X => X)
  
  // [F[X] <: Coll[X]] is treated as a shorthand for [F >: Nothing : [X] =>> Coll[X]] 
  
  type G[-A, +B] 
  opaque type O[X] = List[X] // O is known to be invariant 
  
  type O2[X] = List[X] // X is used covariantly on its right-hand side 
  
  type TL = [A] =>> [B] =>> Map[A, B]
  
  @main def type_lambdas_start(): Unit = {
    
    val k: K[String, Int] = Map("k" -> 3, "m" -> 4)
    
    println(f"k is $k")
    
    val k2: TL[String][Int] = k 
    
    println(f"k2 is $k2")
   
  }
}
