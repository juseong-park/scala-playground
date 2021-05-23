package main

import collections.{EmptyList, Node}

object main extends App {
  val integerList = Node(1, Node(2, Node(3, Node(4, Node(5, EmptyList)))))
  val stringList = Node("A", Node("B", Node("C", Node("D", EmptyList))))

  println(integerList)
  println(integerList.head)
  println(integerList.tail)
  println(integerList.tail.head)
  println(integerList.tail.isEmpty)

  println(stringList)
  println(integerList.addFront(10))

  val doubledList = integerList.map((x: Int) => x * 2)
  val filteredList = integerList.filter((x: Int) => x % 2 == 0)
  val flattedList = integerList.flatMap((x: Int) => Node(x, Node(x + 1, EmptyList)))
  val loweredList = stringList.map((x: String) => x.toLowerCase())

  println(doubledList)
  println(filteredList)
  println(flattedList)
  println(loweredList)

  println(integerList ++ stringList)
  println(integerList == integerList.copy())

  integerList.forEach(println)
  println(integerList.sort((x: Int, y: Int) => y - x))
  println(integerList.zipWith(stringList.addFront("X"), (x: Int, y: String) => x + ":" + y))
  println(integerList.reduce(0)(_ + _))
}
