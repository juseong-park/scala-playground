object Solution {
  def fizzBuzz(n: Int): List[String] = {
    (1 to n).toList.map(i => (i % 3, i % 5) match {
      case (0, 0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case _ => i.toString()
    })
  }
}
