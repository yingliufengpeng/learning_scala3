package scala3_metaprogrmming
import scala.deriving.* 
import scala.compiletime.{erasedValue, summonInline}
 
object derived_demo {
  inline def summonAll[T <: Tuple]: List[Eq[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *:ts ) => summonInline[Eq[t]] :: summonAll[ts]

  trait Eq[T]:
    def eqv(x: T, y: T): Boolean

    extension (x: T)
      def |=| (y: T): Boolean = eqv(x, y)

  object Eq:
    given Eq[Int] with
        def eqv(x: Int, y: Int) = x == y
    
    def check(elem: Eq[_])(x: Any, y: Any): Boolean =
      elem.asInstanceOf[Eq[Any]].eqv(x, y)

    def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

    def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[_]]): Eq[T] =
      new Eq[T]:
        override def eqv(x: T, y: T): Boolean =
          val ordx = s.ordinal(x)
          (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)

    def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[_]]): Eq[T] =
      new Eq[T]:
        override def eqv(x: T, y: T): Boolean =
          iterator(x).zip(iterator(y)).zip(elems.iterator).forall { 
            case ((x, y), elem) => check(elem)(x, y)
          }

    inline given derived[T](using m: Mirror.Of[T]): Eq[T] = 
      lazy val elemInstances = summonAll[m.MirroredElemTypes]
      inline m match
        case s: Mirror.SumOf[T] => eqSum(s, elemInstances)
        case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
 
  
  
  enum Tree[T] derives Eq :
    case Branch(left: Tree[T], right: Tree[T])
    case Leaf(v: T)
  
  def test(): Unit = {
    import Tree.* 
    val eqoi = summon[Eq[Tree[Int]]]
//    val r = eqoi.eqv(Leaf(3), Branch(Leaf(3), Leaf(4)))
    val r = eqoi.eqv(Leaf(3), Leaf(3))
    println(s"r is $r")
  }
  
  @main def derived_demo_start(): Unit = {
    test() 
  }

}
