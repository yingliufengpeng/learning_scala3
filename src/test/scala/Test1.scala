
import org.junit.Test
import org.junit.Assert._
import scala3_decorator.macros.route


class Test1 {
  @Test def t1(): Unit = {

    val r = route("r1", "r2")((t: Int) => t)

//    println(r(3))
    val r2 = r(3)
    println(r2)

  }
}
 