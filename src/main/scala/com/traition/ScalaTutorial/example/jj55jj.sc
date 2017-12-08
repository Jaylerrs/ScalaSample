sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

}
case object End extends Tree[Nothing] {
  override def toString = "."
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](n: Int, value: T) : List(Node[String]) = {

  }
}

//Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
//v55
Tree.cBalanced(4, "x")