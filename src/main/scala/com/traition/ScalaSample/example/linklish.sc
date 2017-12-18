sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "" +    "   \nT(" + value.toString + " " + left.toString + " " + right.toString + ")"
  def isChild:Boolean = left != End() | right != End()
  def isSet:Boolean = !value.equals(())
  def isFullChild = left != End() & right != End()
  def getCost:Int = value match {
    case data:(String,Int) => data._2
    case data:Int => data
    case _ => 0  }}
case class End[+T]() extends Tree[T] {
  override def toString: String = " END "}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End(), End())}

huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))


def huffman(l: List[(String,Int)]): List[(String,String)] = {
  def addNote[T](node: Node[T], list: List[(String,Int)]): Node[Any] = list match {
    case Nil => node
    case head :: tail => {
      if (!node.isSet) {
        addNote(node.copy(value = head), tail)
      }
      else if (node.getCost < head._2)
        addNote(Node(node.getCost + head._2, node, Node(head)), tail)
      else {
        (node.left, node.right) match {
          case (left: Node[Any], right: Node[Any]) => {
            if (right.getCost < head._2)
              addNote(Node(node.getCost + head._2, Node(head), node), tail)
            else {
              val newNode = addNote(left, List(head))
              if (newNode.getCost < right.getCost)
                addNote(node.copy(value = newNode.getCost + right.getCost, left = newNode), tail)
              else
                addNote(node.copy(value = newNode.getCost + right.getCost, left = right, right = newNode), tail)
            }
          }
          case (left: End[Any], right: End[Any]) => {
            if (node.getCost < head._2)
              addNote(Node(node.getCost + head._2, node, Node(head)), tail)
            else
              addNote(Node(node.getCost + head._2, Node(head), node), tail)
          }
          case _ => node
        }
      }
    }
  }
  def decodeNode(node: Node[Any]): List[(String, String)] = node match {
    case Node(value: (String, Int), left: End[Any], right: End[Any]) => List((value._1, ""))
    case Node(value, left: Node[Any], right: Node[Any]) =>
      decodeNode(left).map(f => (f._1, "0" + f._2)) ::: decodeNode(right).map(f => (f._1, "1" + f._2))
    case _ => List((node.toString, ""))
  }
  decodeNode(addNote(Node(),l.sortWith(_._2 < _._2))).sortBy(_._1)
}
