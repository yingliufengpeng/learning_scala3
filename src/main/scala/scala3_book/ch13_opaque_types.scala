package scala3_book

object ch13_opaque_types {

  
  // Scala3 Opaque type aliases provide type abstractions without any overhead 
  
//  class Logarithm(protected val underlying: Double):
//    def toDouble: Double = math.exp(underlying)
//    def + (that: Logarithm): Logarithm =
//      Logarithm(this.toDouble + that.toDouble)
//      
//    def * (that: Logarithm): Logarithm =
//      Logarithm(this.underlying + that.underlying)
//      
//  
//  object Logarithm:
//    def apply(d: Double): Logarithm = new Logarithm(d)
  
  
  trait Logarithms: 
    
    type Logarithm 
    
    // operation on Logarithm 
    def add(x: Logarithm, y: Logarithm): Logarithm
    def mul(x: Logarithm, y: Logarithm): Logarithm
  
    // functions to convert between Double and Logarithm 
    def make(d: Double): Logarithm
    def extract(x: Logarithm): Double 
  
    // extension methods to use `add` and `mul` "methods" on Logarithm 
    extension (x: Logarithm)
      def toDouble: Double = extract(x)
      def + (y: Logarithm): Logarithm = add(x, y)
      def * (y: Logarithm): Logarithm = mul(x, y)
  
  
  // 这种模式的写法的确是很漂亮
  object Logarithms:
    opaque type Logarithm = Double 
    object Logarithm:
      def apply(d: Double): Logarithm = math.log(d)
    
    extension (x: Logarithm)
      def toDouble: Double = math.exp(x)
      def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
      def * (y: Logarithm): Logarithm = x + y 

  object LogarithmsImpl extends Logarithms:
    // vvvvv this is the important difference!
    opaque type Logarithm = Double

    // operation on Logarithm 
    override def add(x: Logarithm, y: Logarithm): Logarithm = make(x.toDouble)

    override def mul(x: Double, y: Double): Double = x + y

    // functions to convert between Double and Logarithm 
    override def make(d: Double): Double = math.log(d)

    override def extract(x: Double): Double = math.exp(x)
   
  // LEAKY ABSTRACTIONS 
  // However, this abstraction is slightly leaky. We have to make sure to only ever program against the abstract
  // interface Logarithms and never diretctly use LogarithmsImpl. Directly using LogarithmsImpl would make the 
  // equality Logarithm = Double visible for the user, who might accidentally use a Double where a logarithmic double 
  /// is expected.
  
  def someComputation(L: Logarithms)(init: L.Logarithm): L.Logarithm = ???
  
  
  // However, outside of the module the type Logarithm is completely encapsulated, or "opaque". To user os Logartithm 
  // it is note possible to discover that Logarithm is actually implemented as a Double 
  
  
  @main def opaque_types_start(): Unit = {
    
//    val l2 = Logarithm(2.0)
//    val l3 = Logarithm(3.0)
//    
//    println((l2 * l3).toDouble)
//    println((l2 + l3).toDouble)
    
    import Logarithms._ 
    val r = Logarithm(3)
    val rr = r + r 
    
    println(f"rr is $rr")
    
    val c: Double = r.toDouble
    
    
  }
}
