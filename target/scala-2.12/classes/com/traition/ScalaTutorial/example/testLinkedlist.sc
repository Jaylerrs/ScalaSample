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
  def cBalanced[T](n: Int, value: T) : Set[Tree[T]] = n match {
    case 0 => Set(End)
    //case 1 => List(Node(value))
    case n if n % 2 == 1 => {
      val subtrees = cBalanced( (n-1) / 2, value)
      //subtrees map (t => Node(value, t, t))
      for {
        t1 <- subtrees
        t2 <- subtrees
      } yield Node(value, t1, t2)
    }
    case n if n % 2 == 0 => {
      val sub1 = cBalanced( (n-1) / 2, value)
      val sub2 = cBalanced( (n-1) / 2 + 1, value)
      (for {
        t1 <- sub1
        t2 <- sub2
      } yield (t1, t2)) flatMap {
        case (t1,t2) => List(Node(value, t1, t2), Node(value, t2, t1))
      }
    }
  }
}

//Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
//v55
Tree.cBalanced(4, "x")



