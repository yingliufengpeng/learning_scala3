package scala3_other_new_feathures

object transparent_trait {

  trait Kind 
  case object Var extends Kind 
  case object Val extends Kind 
  
  val x = Set(if true then Val else Var)
  
  transparent trait S 
  trait Kind2 
  object Var2 extends Kind2, S 
  object Val2 extends Kind2, S 
  val x2 = Set(if true then Val2 else Var2)
  
  // The precise rules are as follows:
  //  when inferring a type of a type variable, or the type of a val 
  // , or the return type 
  // where that type is not higher-kinded
  // and where B is its konwn upper bound or Any if non exists 
  // if the tyep inferred so far is of the form T1 & .. & Tn wher n >= 1 
  // replace n >= 1 ,replace the maxrimal  number of transparent Tis by Any b
  //, while ensuring that the resulting type is still a subtype of the bound B 
  // however, do not perform this widening if al transparent traits Ti can get 
  // replaced in that way.
  
  
  class StringBuilder2(s: String):
    def this() = this("")
  
  @main def transparent_trait_start(): Unit = {
    println(f"x is $x")
    // Constructor proxy companions cannot be used as values by themselves
    // A proxy companion object must be selected with appoly
    
    val r = StringBuilder2()
    val r2 = StringBuilder2("2")
  }
}
