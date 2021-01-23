package scala3_concurrency
import scala.concurrent.Future 
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object main {
  
  def aLongRunningTask(): Future[Int] = {
    Thread.sleep(3000)
    Future(3)
  }
  
  val startTime = System.currentTimeMillis()
  def delta() = System.currentTimeMillis() - startTime 
  def sleep(millis: Long) = Thread.sleep(millis)
  
  
  


  @main def concurency_start(): Unit = {
    val f = aLongRunningTask()
    f.onComplete {
      case Success(value) => println(f"Got the value $value")
      case Failure(e) => e.printStackTrace
    }
    println(f"f is $f")
    
    val ff = f.map(_ * 2) 
    println(f"f2 is $ff")
    
    
    println(f"creating the futures: ${delta()}")
    
    // (1) create the futures
    val f1 = Future { sleep(800); 1}
    val f2 = Future { sleep(200); 2 }
    val f3 = Future { sleep(400); 3 }
    
    // (2) run them simultaneously in a `for` expression 
    val result =  
      for 
        r1 <- f1
        r2 <- f2 
        r3 <- f3 
      yield { 
        println(f"in the `yield` ${delta()}")
        (r1 + r2 + r3)
      }
    
    // (3) process the result 
    result.onComplete {
      case Success(value) => 
        println(f"in the Succes case: ${delta()}")
        println(f"result = $value")

      case Failure(e) =>
        e.printStackTrace()
    }
    
    println(f"before the `sleep(3000)`: ${delta()}")
    
    // important for a littele parallel demo: keep the jvm alive 
    sleep(3000)
    
    
    

  }

}
