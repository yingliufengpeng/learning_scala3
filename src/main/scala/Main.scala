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
 
}


def msg = "ok"