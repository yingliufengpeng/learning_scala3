package scala3_episode.typeclass.numeric

object Scala3Numeric:

  trait Numeric[T]:
    def zero: T

    extension (fa: T)
      def add(fb: T): T

      def mul(fb: T): T
      
      def sub(fb: T): T 
      
      def div(fb: T): T 
     
      def isZero: Boolean  


      def +(fb: T) = fa add fb

      def *(fb: T) = fa mul fb

      def squre: T = fa * fa
  
      def - (fb: T) = fa sub fb 
      
      def / (fb: T) = fa div fb 

  object Numeric:
    def apply[T](using n: Numeric[T]): Numeric[T] = n

    given intNumeric: Numeric[Int] with

      def zero: Int = 0
  
      extension (fa: Int)
        def add(fb: Int): Int = fa + fb

        def isZero: Boolean = zero == fa
        
        def mul(fb: Int): Int = fa * fb

        def sub(fb: Int): Int = fa - fb 
    
        def div(fb: Int): Int = fa / fb 
      

    given num_string: Numeric[String] with

      def zero: String = ""

      extension (fa: String)
        def add(fb: String): String = fa + fb
        def isZero: Boolean = fa == zero 
        def mul(fb: String): String = {
          for
          as <- fa
          bs <- fb
          s <- as.toString ++ bs.toString
            yield
              s
        }
    
        def sub(fb: String): String =
          val newSize = fa.size - fb.size 
          fa.substring(0, newSize)
        def div(fb: String): String =
          val newSize = fa.size / fb.size 
          fa.substring(0, newSize)


    def test(): Unit = {
      val n_s = summon[Numeric[String]]
      val r = "12" * "34"
      println(s"r is $r")

      val r2 = 666.squre
      println(s"r2 is $r2")

    }


  @main def Scala3Numeric_start(): Unit = {
    Numeric.test()
  }
