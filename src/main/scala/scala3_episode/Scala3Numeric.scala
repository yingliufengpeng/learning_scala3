package scala3_episode
 
object Scala3Numeric:

  trait Numeric[T]:
    def zero: T 
    
    extension (fa: T)
      def add(fb: T): T
      def mul(fb: T): T
      

      def + (fb: T) = fa add fb
      def * (fb: T) = fa mul fb
      def squre: T = fa * fa
  
  object Numeric:
    given intNumeric2: Numeric[Int] with
      def zero: Int = 0
      extension (fa: Int)
        def add(fb: Int): Int = fa + fb
        def mul(fb: Int): Int = fa * fb

    given num_string: Numeric[String] with
      override def zero: String = ""
      extension (fa: String)
        def add(fb: String): String = fa + fb
        def mul(fb: String): String = {
          for
            as <- fa
            bs <- fb
            s <- as.toString ++ " " ++ bs.toString
          yield
              s
        }
 
     
    def test(): Unit = {
      val n_s = summon[Numeric[String]]
      val r = "12" * "34"
      print(s"r is $r")
    }


  @main def main_start(): Unit = {
    Numeric.test()
  }