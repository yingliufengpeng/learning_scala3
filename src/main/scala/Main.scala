@main def hello: Unit = {
    
    println("Hello world!")
    
    println(msg)
    
    println(s"expr is $expr")
    
    println(s"find_elem2 is ${find_elem2(3, Array(3, 4))}")
    
    println(s"count iter(1 to 100) is ${count(1 to 100)}")
}

def count[T](arr: Iterable[T]): Int = arr.foldLeft(0)((acc, _) => acc + 1)

def find_elem2[T](elem: T, elems: Iterable[T]): Option[T] =
    Some(elem)

def find_elem(elem: Int, elems: Iterator[Int]): Option[Int] = Some(elem)


def msg = "I was compiled by Scala 3. :)"

def expr = 1 + 2 
