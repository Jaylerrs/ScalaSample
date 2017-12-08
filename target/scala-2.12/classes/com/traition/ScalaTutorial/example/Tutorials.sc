val ioob:String = "The index is out of bound"

val ls: List[Int] = List(1, 1, 2, 3, 5, 8)
val adList: List[Symbol] = List('a, 'b, 'c, 'd)
val afList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f)
val ahList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)

def printList(list: List[Any]): Unit = {
  var int:Int = 0
  print("Print : ")
  while (int < list.length){
    print(" ".concat(list(int).toString))
    int += 1
  }
}

//add list
def addItemToList[A](list: List[A], any: A): List[A] = {
  val newList  = list :+ any
  newList
}

def mergeList(originlist: List[Any], list: List[Any]): List[Any] = {
  val newList = originlist ::: list
  newList
}

/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
val ex1 = last(ls)
def last(list: List[Int]): Int = list(list.length - 1)

/////
val ex2 = penultimate(ls)
def penultimate(list: List[Int]): Int = list(list.length - 2)

/////
val ex3 = nth(2,ls)
def nth(int: Int, list: List[Int]): Int = {
  var length:Int = 0
  while(list(length) != int){
    length+=1
  }
  list(length)
}

/////
val ex4 =lengthOfList(ls)
def lengthOfList(list: List[Int]): Int = list.length

/////
val ex5 = reverse(ls)
def reverse(list: List[Int]): List[Any] = {
  var  listR:List[Int] = List()
  listR = list.reverse
  listR
}

/////
val ex6 = isPalindrome(ls)
def isPalindrome(list: List[Int]): Boolean = {
  var result:Boolean = true
  var num:Int = 0
  while (num < list.length/2){
    if (list(num) != list(list.length - (num +1) )){
      num+=1
      result = false
    }
  }
  result
}

/////
val ex7 = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
def flatten(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case (a: Int) :: tail => a :: flatten(tail)
  case (a: List[Any]) :: tail => flatten(a) ::: flatten(tail)
  case _ :: tail => flatten(tail)
}

/////
val ex8 = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

def compress(list: List[Any]): List[Any] ={
  var newList:List[Any] = List()
  var lastElement:Any = ""
  var number:Int = 0
  while (number < list.length){
    if(number != 0){
      if (lastElement != list(number)){
        lastElement = list(number)
        newList = addItemToList(newList, lastElement)
      }
    } else {
      lastElement = list(number)
      newList = addItemToList(newList, lastElement)
    }

    number +=1
  }
  newList
}

/////
val ex9 = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e,
  'e, 'e, 'e))
def pack[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}
///// ex9
object P09 {
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}

/////
val ex14 = duplicate(adList)
def duplicate[A](list: List[A]): List[A] = {
  list.flatMap(x => List.fill(2)(x))
}

/////
val ex15 = duplicateN(3, adList)
def duplicateN[A](int: Int, list: List[A]): List[A] = {
  list.flatMap(x => List.fill(int)(x))
}
/////
val akList: List[Any] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
val ex16 = drop(3, akList)
def drop[A] (int: Int, list: List[A]): List[A] = {
  if (int < list.size){
    val newList = list.take(int - 1) ++ list.drop(int)
    newList
  } else {
    print(ioob)
    null
  }

}

/////
val ex17 = split(3, akList)
def split(int: Int, list: List[Any]): List[Any] = {
  if (int < list.size){
    var newList:List[Any] = List()
    newList = addItemToList(newList, list.take(int))
    newList = addItemToList(newList, list.drop(int))
    newList
  } else {
    print(ioob)
    null
  }
}

/////
val ex18 = slice(3, 7, akList)
def slice(x:Int, y:Int, list: List[Any]): List[Any] = {
  if (x > 0 && y > x && y < list.size){
    val xList = list.drop(x)
    val yList = xList.take(y-x)
    yList
  } else {
    print(ioob)
    null
  }
}

/////
val ex19 = rotate(3, akList)
def rotate(int: Int, list: List[Any]): List[Any] = {
  var  n:Int = int
  if (int < 0){
    n = list.size + int
  }
  var nList:List[Any] = List()
  var hList:List[Any] = List()
  var tList:List[Any] = List()
  if (n < list.size && n >= 0){
    tList = list.take(n)
    hList = list.drop(n)
    nList = hList ::: tList
    nList
  } else {
    print(ioob)
    null
  }

}
val ex19_2 = rotate(-2, akList)

/////
val ex20 = removeAt(1, List('a, 'b, 'c, 'd))
def removeAt(int: Int, list: List[Any]):List[Any] = {
  if (int < list.size){
    val ra = list(int)
    val tempList = drop(int+1, list)
    var nList:List[Any] = List()
    nList = addItemToList(nList, tempList)
    nList = addItemToList(nList, ra)
    nList
  } else {
    print(ioob)
    null
  }
}

/////
val ex21 = insertAt('new, 1, List('a, 'b, 'c, 'd))
def insertAt(any: Any, int: Int, list: List[Any]): List[Any] = {
  if (int <= list.size){
    val hl:List[Any] = list.take(int)
    val tl:List[Any] = list.drop(int)
    val nl:List[Any] = hl ::: any :: tl
    nl
  } else {
    print(ioob)
    null
  }
}

/////
val ex22 = range(4,9)
def range(x: Int, y: Int): List[Int] = {
  if (y > x){
    val nl:List[Int] = List.range(x,y+1)
    nl
  } else {
    print("Invalid input")
    null
  }
}

/////
val ex23 = randomSelect(3, ahList)
def randomSelect(int: Int, list: List[Any]):List[Any] = {
  var i:Int = 0
  var num:Int = 0
  var rl:List[Int] = List()
  var r = scala.util.Random
  var nl:List[Any] = List()
  while (i < int){
    num = r.nextInt(int)
    if (!(rl.contains(num))){
      nl = addItemToList(nl, list(num))
      rl = addItemToList(rl, num)
      i+=1
    }
  }
  nl
}

/////
val ex24 = lotto(6, 49)
def lotto(x:Int, y:Int):List[Int] = {
  var i:Int = 0
  var lottoList:List[Int] = List()
  var r = scala.util.Random
  while (i < x){
    val num:Int = r.nextInt(y)
    if (!(lottoList.contains(num))){
      lottoList = addItemToList(lottoList, num)
      i+=1
    }
  }
  lottoList
}

/////
val  ex25 = randomPermute(afList)
def randomPermute(list: List[Symbol]): List[Symbol] = {
  var i:Int = 0
  var nl:List[Symbol] = List()
  var numList:List[Int] = List()
  var r = scala.util.Random
  while (i < list.size){
    val num:Int = r.nextInt(list.size)
    if (!(numList.contains(num))){
      numList = addItemToList(numList, num)
      nl = addItemToList(nl, list(num))
      i+=1
    }
  }
  print("Original : ")
  printList(list)
  println()
  nl
}

/////
val ex26 = combinations(3, afList)
def flatMapSubLists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
  ls match {
    case Nil => Nil
    case sublist@(_ :: tail) => f(sublist) ::: flatMapSubLists(tail)(f)
  }

def combinations[A](n: Int, ls: List[A]): List[List[A]] =
  if (n == 0) List(Nil)
  else flatMapSubLists(ls) { sl =>
    combinations(n - 1, sl.tail) map {sl.head :: _}
  }

/////
//val ex27 = group(List(2, 2, 5), List("Aldo",
// "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//def group3[A](ls: List[A]): List[List[List[A]]] =
//  for {
//    a <- combinations(2, ls)
//    noA = ls -- a
//    b <- combinations(3, noA)
//  } yield List(a, b, noA -- b)
//
//def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
//  case Nil     => List(Nil)
//  case n :: ns => combinations(n, ls) flatMap { c =>
//    group(ns, ls -- c) map {c :: _}
//  }
//}

/////
val ex28 = lSort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e),
    List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
def lSort[A](list: List[A]):List[A] = {
  null
}

//29

//30

/////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////