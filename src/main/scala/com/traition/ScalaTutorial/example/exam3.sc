object Logic {
  type T = Boolean

  def and(a: T, b: T) = a && b
  def nand(a: T, b: T) = !and(a,b)
  def or(a: T, b: T) = a || b
  def nor(a: T, b: T) = !or(a,b)
  def xor(a: T, b: T) = a != b
  def impl(a: T, b: T) = !xor(a,b) //xnor
  def equ(a: T, b: T) = a == b
}

def table2(f: (Boolean, Boolean) => Boolean) {
  println("A     B     result")
  for (
    a <- List(true, false);
    b <- List(true, false)
  ) {
    printf("%-5s %-5s %-5s\n", a, b, f(a, b))
  }
}

def gray(int: Int) : Unit = {
  val x1 = (0 to (Math.pow(2,int).toInt)-1).toList
  val num = "%0"+int+"d"
  x1.foldLeft(List[String]()){
    (s,i) => {
      s:+i.toBinaryString
    }
  }.map(x => num.format(x.toInt)).toString().foreach(print)
}


//46
Logic.and(true,true)

//47
table2((a: Boolean, b: Boolean) => a && (a || !(b)))

//49
gray(4)

val v50 = huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))

case class Node(value: String="", freq: Int=0,
                left: Option[Node]=None,
                right: Option[Node]=None)

def huffman(l:List[(String,Int)]):List[(String,String)]={
  def setData (list:List[(String,Int)]):List[(Node)]={
    //list.map(x=>(x._1,x._2,Node(x._1,x._2,None,None)))
    list.map(x=>(Node(x._1,x._2,None,None)))
  }
  def toHuffTree(list: List[Node]):List[Node]=list match {
    case Nil=> list
    case list =>{
      list.sortBy(x=>x.freq) match {
        case Nil=>list
        case _::Nil => list
        case head::tail => toHuffTree(Node(head.value+tail.head.value,head.freq+tail.head.freq,Some(head),Some(tail.head))::tail.drop(1))
      }
    }
  }
  def decodeHuffTree(node: Node):List[(String,String)]=node match {
    case Node(value,_  ,None,None) => List((value,""))
    case Node(_,_,_,_) =>
      decodeHuffTree(node.left.orNull).map(f => (f._1, "0" + f._2)) ::: decodeHuffTree(node.right.orNull).map(f => (f._1, "1" + f._2))
    case _ => List((node.toString, ""))
  }
  decodeHuffTree(toHuffTree(setData(l))(0)).sortBy(x=>x._1)
}