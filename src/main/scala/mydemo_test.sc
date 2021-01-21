val nums = List(1, 2, 3)
case class Person(firstName: String, lastName: String)

val p = Person("Martin", "Odersky")

nums.map(i => i * 2)
nums.map(_ * 2)

nums.filter(i => i > 1)
nums.filter(_ > 1)

val s = "Hello"
val p = Person("AI", "Pacino")
val sum = nums.reduceLeft(_ + _)
val y = nums.map(_ * 2)
val z = nums
  .filter(_ > 100)
  .filter(_ < 10_000)
  .map(_ * 2)









