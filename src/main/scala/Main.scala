@main def hello: Unit = {
    
    trait Foo 
    given foo: Foo with {
        override def toString: String = "foo"
    }
    
    extension [T](x: T)(using foo: Foo)
        def show = println(f"ok $x $foo")
    
    println("Hello world!")
    
    val r = 20
    r.show 
    "wangpeng".show 
    
    
    given Int = 3
    given Double = 45
    
    val rr = summon[Int]
    val rr2 = summon[Double]
    
    println(f"rr is ${rr}, rr2 is ${rr2}")
    
 
}


def msg = "ok"