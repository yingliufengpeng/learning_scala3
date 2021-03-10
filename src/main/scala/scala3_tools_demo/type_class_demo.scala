package scala3_tools_demo

object type_class_demo {

//  trait SemiGroup[T]:
//    extension (x: T) def combine (y: T): T
//
//  trait Monoid[T] extends SemiGroup[T]:
//    def unit: T
//
//  given Monoid[String] = new Monoid[String] {
//    extension (x: String)
//      def combine (y: String): String = x.concat(y)
//    def unit: String = ""
//  }
   
  
  trait Numeric[T]:
    def zero: T
    
    extension (fa: T) 
      def add(fb: T): T 
      def mul(fb: T): T 
      
      def + (fb: T): T = add(fb)
      def * (fb: T): T = mul(fb)
  
      def square: T = fa * fa 
    
  
  object Numeric:
    given Numeric[Int] = new Numeric[Int] {
      override def zero: Int = 0
      extension (fa: Int)
        def add(fb: Int): Int = fa + fb
        def mul(fb: Int): Int = fa * fb
    }

    given num_string: Numeric[String] = new Numeric[String] {
      override def zero: String = ""
      extension (fa: String)
        def add(fb: String): String = fa + fb
        def mul(fb: String): String = {
          for 
            as <- fa 
            bs <- fb 
            s <- as.toString ++ bs.toString 
          yield 
            s
        }

    }
  
  
    def sumList[T](ts: List[T])(using numeric: Numeric[T]): T =
      ts.reduce((a, b) => a mul b)
      
    def test(): Unit = {
      val r = "14"
      val r2 = r.square
      println(s"r is $r2")

      val r3 = for
        as <- r
        bs <- r
        s <- as.toString ++ bs.toString
      yield
        List(s)
      println(s"r3 is $r3")
    }



  @main def type_class_demo_start(): Unit = {
    {
      val r = ( 1 to 10).toList
      val r2 = Numeric.sumList(r)
      println(s"r2 is $r2")
    }
    
    {
      val r = (1 to 10).toList.map(_.toString)
      val r2 = Numeric.sumList(r)
      println(s"r2 is $r2")
    }
    
    Numeric.test()
  }

}
