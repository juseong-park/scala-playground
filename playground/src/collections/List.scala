package collections

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def addFront[B >: A](x: B): MyList[B]
  def stringify: String
  override def toString: String = "[" + stringify + "]"

  def ++[B >: A](list: MyList[B]): MyList[B]
  def map[B](trans: A => B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def flatMap[B](trans: A => MyList[B]): MyList[B]
}

case object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def addFront[B >: Nothing](x : B): MyList[B] = Node(x, EmptyList)
  override def stringify: String = ""

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  def map[B](trans: Nothing => B): MyList[B] = EmptyList
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = EmptyList
  def flatMap[B](trans: Nothing => MyList[B]): MyList[B] = EmptyList
}

case class Node[+A](first: A, left: MyList[A]) extends MyList[A] {
  def head: A = first
  def tail: MyList[A] = left
  def isEmpty: Boolean = false
  def addFront[B >: A](x: B): MyList[B] = Node(x, this)
  override def stringify: String =
    if (left.isEmpty) "" + head
    else first + ", " + left.stringify

  def ++[B >: A](list: MyList[B]): MyList[B] = Node(first, left ++ list)
  def map[B](trans: A => B): MyList[B] = Node(trans(first), left.map(trans))
  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(first)) Node(first, left.filter(predicate))
    else left.filter(predicate)
  def flatMap[B](trans: A => MyList[B]): MyList[B] = trans(first) ++ left.flatMap(trans)
}
