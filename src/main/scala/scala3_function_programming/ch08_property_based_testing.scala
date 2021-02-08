package scala3_function_programming

import scala3_function_programming.ch06_general_state.{RNG, SimpleRNG, State}
import scala3_function_programming.ch07_purely_funtiaonal_parallelism.Parallel
import scala3_function_programming.ch08_property_based_testing.Result.{FailedCase, SuccessCount}

import java.util.concurrent.Executors

object ch08_property_based_testing {
  /**
   * 
   * val intList = Gen.listOf(Gen.choose(0, 100)) 
   * val prop = foreAll(intList)(ns => ns.sum() == ns.reverse.sum()  ) 
   * val prop2 = foreAll(intList)( ns => ns.reverse.max() == ns.reverse() == ns.shuffle().max() )
   * 
   */
  
  enum A:
    case B(v: Int => Int)
    case A_sentail 
  
  trait Consumer[-T]:
    def consume(v: T): Unit 
  
  trait Producer[+T]:
    def producer: T 
  
  type Cons = [T] =>> T => Unit 
  type Pro = [T] =>> () => T 
  
   
  enum Result:
    case Passed
    case Falsifield(failure: FailedCase, successes: SuccessCount, indexs: List[Int] = Nil)
  
    def isFalsified: Boolean = this match
      case Passed => false
      case _ => true 



  object Result:
    type MaxSize = Int 
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
  
  
  import Result._
  
  // SGEN则是对所生成的Gen进行大小限制的写法 // Int => Gen[A]  这样的数据结构,这种数据结构的思路是在哪里体现这种模式呢?
  // 通过另一个个外置的,嵌套的想法来做对数据生成流的处理的过程
  case class SGen[+A](forSize: Int => Gen[A]):
    self =>
    def map[B](f: A => B): SGen[B] =
      SGen(
        n => 
          self.forSize(n).map(f)
      )
      
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen {
        n =>
          self.forSize(n).flatMap(e => f(e).forSize(n))
      }
      
    // 在执行体中进一步的对所有的执行体进行汇总操作
    def forAll(f: A => Boolean): Prop =
      Prop {  // 这里的Prop借用了SGen中的计算流方法,来构建每次计算的流处理的过程, 这个方法也是执行流开始的地方!!!
        (max, n, rng) =>          
          val casesPersize = (n + (max - 1) ) / max
//          println(s"casePersize is $casesPersize")
          val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1)
            .map(i => self.forSize(i).forAll(f)) // Genp[A]然后生成Prop

          // 批量处理运行测试代码的逻辑,并最终对这些代码的逻辑进行汇总 
          // 由此可知,之前生成的p本身计算的逻辑,该计算逻辑的输入是通过map方法中的参数进行设定
          val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPersize, rng) })
            .reduceLeft(_ && _)

          prop.run(max, n, rng)

      }
      
      
  // 很显然 Gen是一个可用的数据类型,该数据类型中有一次嵌套一层State数据类型,并最终存储的是一个函数 S => (A, S) 类型
  // Gen的数据类型中并没有指定由多少个数据生成,所以SGen来指定数据的生成
  case class Gen[+A](sample: State[RNG, A]):
    self => 
  
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))
    
    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))
      
    
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(f(_).sample))
    


    // 从这个函数的签名来看,这个应该是无穷的
    def unsized: SGen[A] =
      SGen {
        _ =>
          Gen(State {
            rng =>
              (randomStream(rng).head, rng)
          })
      }
    
      
    
    // 就是用于显示生成的数据
    def listOf: SGen[List[A]] =
      SGen {
        n =>
          Gen(State {
            rng =>
              
              (randomStream(rng).take(if n < 1 then 1 else n).toList, rng)
          })
      }

 
    // 生成这些可用的测试执行体,然后对对这样执行体再进一步做汇总,聚合的操作
    def forAll(f: A => Boolean): Prop =
      Prop { // Prop只是借用了Gen中的计算方法,来做Gen中的数据的生成
        (max, n, rng) =>
//          println(f"n is $n") 切记只是相同的函数签名,但是不同的实现的函数的的执行体
          randomStream(rng).zip(LazyList.from(0)).take(n).map {
            case (a, i) => 
              try
                if f(a) then Passed else Falsifield(a.toString, i, List(i))
              catch
                case e: Exception => Falsifield(buildMsg(a, e), i)  
              
          }.find(_.isFalsified).getOrElse(Passed)
      }
      
  
    def randomStream(rng: RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(sample.run(rng)))
      
    
    def buildMsg[A1 >: A](s: A1, e: Exception): String =
      s"test case: $s\n" + 
        s"generated an exception: ${e.getMessage}\n" + 
        s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
        
  object Gen:

    def choose(start: Int, stopExclsive: Int): Gen[Int] = {
      Gen(State {
        rng =>
          if start >= stopExclsive then
            (start, rng)
          else
            val (r0, rng0) = rng.nextInt
            val r1 = math.abs(r0) % (stopExclsive - start) + start
//            println(f"r1 is $r1")
            (r1, rng0)
      })
    }
  
  
    def unit[A](a: => A): Gen[A] =
      Gen(State( s => (a, s)))
  
  
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(e => if e then g1 else g2)
  
    // 概率算法目前不会使用
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val sum = g1._2 + g2._2 // 不考虑为负值的情况
      val probability_1 = g1._2 / sum
      val probability_2 = g2._2 / sum
      for
        i <- g1._1
        j <- g2._1
      yield
          i
    }
    
    
    def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
      for 
        i <- a 
        j <- b 
      yield 
        f(i, j)
        
    
    def **[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] =
      map2(a, b)((_, _))
      
    
    def genSTringIntFn(g: Gen[Int]): Gen[String => Int] =
      g map (i => s => i)
  
    def int: Gen[Int] =
      Gen(State {
        rng =>
          rng.nextInt
      })
    
    def double: Gen[Double] =
      int.map(_.toDouble)
  
  
    def boolean: Gen[Boolean] =
      choose(0, 2)
        .map(v => if v == 0 then
          false else
          true
        )
  
  
  import Result._ 
  
  case class Prop(run: (MaxSize, TestCases, RNG) => Result): 
    self => 
    type Self = Prop 
      
    // && 和 || 这两者之间的思路设计的不是很对,后续再做调整
    def &&(that: Self): Self = 
      Prop {
        (max, n, rng) =>
          (self.run(max, n, rng), that.run(max, n, rng))  match
            case (Passed, Passed) => Passed
            case (Falsifield(s0, index0, _), Passed) => Falsifield(s0, index0) 
            case (Passed, Falsifield(s1, index1, _)) => Falsifield(s1, index1)
            case (Falsifield(s0, index0, _), Falsifield(s1, index1, _)) => Falsifield(s"$s0\n$s1", index0 + index1)
         
      }
       
    
    def ||(that: Self): Self =
      Prop {
        (max,n, rng) =>
          self.run(max, n, rng) match
            case Passed => Passed
            case _ => that.run(max, n, rng)
      }
      
  object Prop:
    Prop_Self =>
    def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(100)): Unit =
      p.run(maxSize, testCases, rng) match
        case Falsifield(msg, n, _) =>
          println(s"! Falsified after $n passed tests(某些测试已经通过!!!): \n$msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests")
    
    def check2(p: => Boolean): Prop =
      lazy val resut = p 
      Gen.unit(()).forAll(_ => resut)
  
    def check(p: => Boolean): Prop =
      Prop {
        (_, _, _) =>
          if (p) then Passed else Falsifield("()", 0)
      }
      
   
    object GenPar:
      import concurrent.ExecutionContext.Implicits.global
//      import concurrent.ExecutionContext.fromExecutorService
      import concurrent.JavaConversions.asExecutionContext

      import Parallel.{_}
      val S = Gen.weighted( 
        Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
        Gen.unit(Executors.newCachedThreadPool()) -> 0.25,
        
      )
      
      extension [A] (gen: Gen[A])
        def forAllPar(f: A => Par[Boolean]): Prop =
          Gen.**(S, gen).forAll {
            case (s, a) => f(a)(using s).get 
          }
      

      def test(): Unit = {
        val r = unit(2)
        val p3 = check {
          equal(unit(1).map(_ + 1), unit(2)).get 
        }
        
        val pint = Gen.choose(0, 100).map(unit(_))
        val p4 = pint.forAllPar(n => equal(n.map(y => y), n))
      }


  @main def property_based_testing_start(): Unit = {
    
    val smallInt = Gen.choose(1, 10)
    val maxProp = smallInt.listOf.forAll { ns =>
      val max = ns.max 
      !ns.exists(_ > max)
    }
    
    Prop.run(maxProp, maxSize = 10, testCases = 10)
    
    val sortedProp = smallInt.listOf.forAll {
      ns =>
        ns.sorted.sorted.toList == ns 
    }
    
    val r4 = Prop.check(true)
    Prop.run(r4)
    
    Prop.GenPar.test()
    
  }

}
