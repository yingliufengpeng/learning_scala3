package scala3_other_new_feathures

object explicit_nulls {
  
  class C:
    lazy val f: String = foo("f")
    def foo(f2: String): String = f2 
//    @NotNull val name: String = ""
  
  
  extension [T](x: T | Null)
    inline def nn: T = {
      assert(x != null)
      x.asInstanceOf[T]
    }
  
  
  class C2(val x: Int, val next: C2 | Null)
  @main def explicit_nulls_start(): Unit = {
    
    val x: String = null 
    
    println(f"x is $x")
    
    val x2: String | Null = null 
    
    println(f"x2 is $x2")
    
    val r3: Object = null 
    println(f"r3 is $r3")
    
    val r4 = null 
     
    val m = 0
    val y: String = "3"
    val z: String | Null = "null" 
    
    println(y == z)
    println(y == null)
    println((0: Any) == null)
    
    z.nn 
    
    // Suppose we have a BoxFactory[String]. Notice that calling make
    // makeBox() on it returns a Boxj[String] | NuchcekedNull, not a 
    // Box[String | UncheckedNull] | UncheckedNull. This seems at 
    // first glance unsound, but is sound because calling get() on 
    // a Box[String] returns a String|UncheckedNull
    // Notice that we need to patch all Java-defined classes that transitively
    // appear in the argument or return type of a field or method accessible
    // from the scala code beging compiled. Absent crazy reflection magic, 
    // we think that all such Java classws must be visible to the Typer
    // This seems at first glance unsound ("What if box itself has null inside?")
    //, but is sound because calling get() on a Box[String] returns a 
    // String | UncheckedNull 
    
    // Notice that we need to patch all Java-defined classes that transitively
    // appear in the argument or return type of a field or method accessible from 
    // the Scala code being compiled. Absent crazy reflection magic. We think that 
    // all such Java classes must be visible to the Typer in the first place, so 
    // they will patched.
    
    // We will append UncheckedNull to the type arguments if the generic class is 
    // defined in Scala.
    
    // In this case, since Box is Scala-defined, we will get Box[T|UncheckedNull] |
    // UncheckedNull. This is needed because our nullability function is only applied
    // to the Java class, but not to the Scala ones, so we need a way to tell Box that
    // Box that it contains a nullable value
    
    // The List is Java-defined, so we don't append UncheckedNull to its typs arguments
    //. But we still need to nullify its inside.
    // we don't nullify simpole literal contant (final) fileld, since they are known 
    // to be non-null
    
    
    
    val s: String | Null = "44"
    if s == null then 
      println("null")
    else
      println(f"s'length is ${s.length}")
    
    if s != null then 
      println(f"s'length is ${s.length}")
    else
      println(f"null")
      
    val s3: String | Null = "k"
    val s4: String | Null = "k"
    
    if s3 != null && s4 != null then 
      val r = s3 + s4 
    else
      val r = 0
    
    
    if s3 == null || s4 == null then 
      val r = 3
    else
      val r = 4
    
    val s5: String | Null = "44"
    
    if s5 != null && s5.length > 0 then 
      println(s)
    else
      println("kk")
      
    if s5 == null || s5.length > 0 then 
      val r = 4
    else
      println("kkk")
      
    val s6: String | Null = "kk"
    
    s6 match
      case _: String => println("s6")
      case null => println("()")
 
//    
    var xs: C2 | Null = C2(1, C2(2, null))
    // xs is trackable, since all assignments are in the same method 
    if xs != null then   
      println(xs)
 
      
    
  }

}
