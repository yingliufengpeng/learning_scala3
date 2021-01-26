package scala3_other_new_feathures

object kind_polymorphism {
  
  // Normal are subtypes of Any, covariant single argument type constructors
  // such as List are subtypes of [+] =>> Any, and the Map type constructor is 
  // a subtype of [X, +Y] =>> Any 
  
  def f[T <: AnyKind]() = ()
  
  // A type can be used only as prescribed by its kind. Subtypes of Any cannot 
  // be appolied to type arguments whereas subtypes of [X] =>> Any must 
  // be applied to a type argument, unless they are passed to type 
  // parameters of the same kind.
  
  @main def kind_polymorphism_start: Unit = {
    f[Int]()
    f[List[Int]]()
    f[List]()
    f[[X] =>> String]()
    
  }
}
