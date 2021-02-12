package scala3_function_programming

object ch06_general_state {
  
  // State is short for computation that carries some state along, or state action, state transition 
  // eor even statement(see the next section)
  //  type State[S, +A] = S => (A, S)
  
  trait RNG: 
    def nextInt: (Int, RNG)

  
  case class SimpleRNG(seed: Long) extends RNG:
    override def nextInt: (Int, RNG)  =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  
  
  case class State[S, +A](run: S => (A, S)):
    def flatMap[B](f: A => State[S, B]): State[S, B] = 
       State {
         (s: S) =>
           val (a0, s0) = run(s)
           val (b1, s1) = f(a0).run(s0)
           (b1, s1)
       }
       
    
    def map[B](f: A => B): State[S, B] =
      flatMap(e => State(s => (f(e), s)))

    
    def ints(n: Int): State[S, List[A]] =
      State {
        s =>
          val r = List.fill(n)(this)
          State.sequence(r).run(s)
      }
      
    
    def get[S]: State[S, S] = State(s => (s, s))
    
    
    def set(s: S): State[S, Unit] = State(_ => ((), s))
  
    
    def modity(f: S => S): State[S, Unit] =
      for 
        i <- get 
        _ <- set(i)
      yield 
        ()  

  
  type Rand = [A] =>> State[RNG, A]
  
  
  object State:
    def unit[S, A](v: A): State[S, A] = State(s => (v, s))
    
    def get[S]: State[S, S] = run(s => (s, s))
    
    def set[S](v: S): State[S, S] = run(s => (s, v ))
    
    
    def run[S, A](f: S => (A, S)) = State(f)
  
    
    def map2[A, B, C](left: Rand[A], right: Rand[B])(f: (A, B) => C): Rand[C] =
      for 
        i <- left 
        j <- right
      yield 
        f(i, j)

    
    def map3[A, B, C](left: Rand[A], right: Rand[B])(f: (A, B) => C): Rand[C] =
      left.flatMap(i => right.map(j => f(i, j)))
      

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      run {
        s =>
          fs.foldLeft((List.empty[A], s)){ case ((list_r0, s0), sta) =>
            val (r, s1) =  sta.run(s0)
            (r :: list_r0, s1)
          }
      }
     
  
  def map[S, A, B](state_f: S => (A, S))(f: A => B): S => (B, S) =
    s => 
      val (a, s0) = state_f(s)
      (f(a), s0)
  
  
  enum Input:
    case Coin 
    case True 
  
  
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  
  
  object M:
    import Input._
    type M = State[Machine, (Int, Int)]

    
    def operation(x: Input): State[Machine, Unit] = State.run { s => 
      (x, s) match
        case (Coin, Machine(true, cadies, coins)) if cadies > 1 => ((), Machine(false, cadies , coins + 1 ) ) 
        case (True, Machine(false, cadies, coins )) => ((),  Machine(true, cadies - 1, coins))
        case (x0, s0) => ((), s0)
    }
  
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      State.run {
        s =>
          val r = inputs.map(operation)          
          val (_, rr)  = State.sequence(r).run(s)
          ((rr.coins, rr.candies), rr)
      }
      
    
    def test(): Unit = {
      val machine = Machine(true, 10, 10)
      val r = simulateMachine(List(Coin, True, Coin, True, True, True, Coin, True, Coin, True)).run(machine)
      println(f"r is $r")
    }


  @main def general_start: Unit = {
    val rng = SimpleRNG(42)
    
    val r0 = State.run((s: RNG) => s.nextInt)
    
    val r = State.unit(3).run(rng)
    println(f"r is $r")
    
    val r2 = r0.ints(2).run(rng)
    println(f"r2 is $r2")
    
    M.test()
  
  }
}
