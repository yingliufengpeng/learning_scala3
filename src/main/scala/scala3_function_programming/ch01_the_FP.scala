package scala3_function_programming

import scala3_function_programming.ch01_the_FP.Cafe.Coffee

object ch01_the_FP {
  // Side effect
  // Modifying a variable 
  // Modifying a data structure in place
  // Setting a field on an object
  // Throwing an exception or halting with an error
  // Printing to the console or leading user input
  // Reading from or writing to a file 
  // Drawing on the screen.
  
  object Cafe:
  
    trait Payments:
      def charget(cc: CreditCard, price: Double): Unit 
    
    case class Coffee(price: Double = 8.8)
    
    class CreditCard:
      def charge(price: Double): Unit = ???
    
    class Cafe:
      
      def buyCoffee(cc: CreditCard, p: Payments): Coffee =
        val cup =  Coffee() 
        p.charget(cc, cup.price)
        cup
  
  object Cafe2:
    
    class CreditCard
    
    case class Coffee(price: Double = 8.8)
    
    case class Charge(cc: CreditCard, amount: Double):
      def combine(other: Charge): Charge =
        if cc == other.cc then 
          Charge(cc, amount + other.amount)
        else
          throw Exception(s"Can't combine charges to different cards")
          
    class Cafe:
      // 把不同的信用卡的charge合并到一起
      def calesce(charges: List[Charge]): List[Charge] =
        charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
      
      def buyCoffee(cc: CreditCard): (Coffee, Charge) =
        val cup = Coffee() 
        (cup, Charge(cc, cup.price))
  
      def buyCoffes(cc: CreditCard, n: Int): (List[Coffee], Charge) =
        val purchases = List.fill(n)(buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
    
   
  @main def the_FP(): Unit = {
    
  }

}
