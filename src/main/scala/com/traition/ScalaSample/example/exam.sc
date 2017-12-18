val num = List(1,1,2,3,5,8)

//1
val v1 = last(num)
def last(a:List[Int]) : Int = {
  a.last
}

//2
val v2 = penultimate(num)
def penultimate(b : List[Int]) : Int = {
  b(b.length-2)
}

//3
val Kth = 2
val v3 = nth(Kth,num)
def nth(a : Int ,b : List[Int]) : Int = {
  b(a)
}

//4
val v4 = length(num)
def length(b : List[Int]) : Int = {
  b.length
}

//5
val v5 = reverse(num)
def reverse(b : List[Int]) : List[Int] = {
  b.reverse
}

//6
val numpali = List(1,2,3,2,1)
//val x = numpali.length/2
val v6 = isPalindrome(numpali)
def isPalindrome(b : List[Int]) : Boolean = {
  numpali.equals(numpali.reverse)
}

//7
val v7 = (List(List(1,1),2,List(3,List(5,8,List(2,2)))))

def flat(b : List[Any]): List[Any]= b.flatten {
    case c: List[Any] =>  flat(c)
    case d => List(d)
}
flat(v7)

def pack(list: List[Symbol]): List[List[Symbol]] = list match {
  case Nil=>Nil
  case _ => {
    val (done, left) = list.span(_ == list.head)
    (done) +: pack(left)
  }
}

//8
val v8 = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
def compress(b : List[Symbol]): List[Symbol] = {
  pack(b).map(x => x(0))

//  b.foldLeft(List[Symbol](b.head)) {
//    (s, i) => {
//      //if (s.isEmpty) List(i)
//      if(s.last == i) s
//      else s :+ i
//    }
//  }
}


//9
val v9 = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))


//10
val v10 = encode(v9)
def encode(b : List[List[Any]]): List[(Int,Any)] = {

  b.map(x => (x.length,x(0)))
//  b.foldLeft((List[(Int,Any)]())){
//    (s, i) =>{
//      s :+ (i.length,i(0))
//    }
//  }
}

//11
val v11 = encodeModified(v10)
def encodeModified(b : List[Any]): List[Any] = {
  b.map{
    case (1, y:Symbol) => y
    case (x:Int, y:Symbol)=>(x,y)
  }
}

//12
val v12 = decode(v10)
def decode(list: List[(Int,Any)]) : List[Any] = {
  list.flatMap(x => List.fill(x._1)(x)).map( y => y._2)

//  list.foldLeft(List[Any]()){
//    (s,i) =>{
//
//      val x2 = (for (j <- 1 to i._1) yield i._2).toList
//
//      s ::: x2
//    }
//  }
}

//13
val v13 = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
def encodeDirect(list : List[Any]) : List[(Int,Any)] = {
  var num = 1

  var c = list.length
  //print(x)
  val listAns = list.foldLeft((List[Any](), List[Int]())){
    (y,i) =>{

      c = c-1

      val (s, x) = y
      if (s.isEmpty) (List(i), x)
      else if(s.last != i){
        val ss = s :+ i
        val xx = x :+ num
        num = 1
        (ss, xx)
      }
      else{
        num = num+1
        if(c == 0){
          val ss = s
          val xx = x :+ num
          num = 1
          (ss, xx)
        }
        else
          (s, x)
      }

    }
  }

  val listAnsReal = listAns._2.zip(listAns._1)

  listAnsReal
}

//14
val v14 = duplicate(List('a, 'b, 'c, 'c, 'd))

def duplicate(list: List[Symbol]) : List[Symbol] ={
  list.flatMap(x => List.fill(2)(x))

//  list.foldLeft(List[Symbol]()) {
//    (s, i) => {
//      s :+ i :+ i
//    }
//  }

  // list.map(x=>List(x,x)).flatten
}

//15
val v15 = duplicateN(3, List('a, 'b, 'c, 'c, 'd))

def duplicateN(N : Int ,list: List[Symbol]) : List[Symbol] ={
  list.flatMap(x => List.fill(N)(x))
}

//16
val v16 = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
def drop(n : Int, list: List[Symbol]) : List[(Symbol)]= {
  list.zipWithIndex.filter(x => (x._2+1)%3 != 0).map( y=> y._1)
}

//17
val v17 = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//(List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
def split(n:Int,list: List[Symbol]) : (List[Symbol],List[Symbol]) = {
    (list.dropRight(list.length-n),list.drop(n))
}

//v18
val v18 = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g)
def slice(I:Int ,K:Int ,list: List[Symbol]) : List[Symbol] = {
  val b1 = list.dropRight(list.length-K) // delete 'h, 'i, 'j, 'k
  val b2 = b1.drop(I) // delete 'a, 'b, 'c
  b2
}


//v19
def rotate(n:Int, list: List[Symbol]) : List[Symbol] = {
  if(n>0){
    val b1 = list.drop(n)
    val b2 = b1 ::: list.dropRight(list.length-n)
    return b2
  }
  else{
    val b1 = list.dropRight(n.abs)
    val b2 = list.drop(list.length - n.abs) ::: b1
    return b2
  }
}
//v19.1
val _1v19 = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//v19.2
val _2v19 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)


//v20
val v20 = removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
def removeAt(n:Int , list: List[Symbol]) : (List[Symbol],Symbol) = {
  (list.take(n):::list.drop(n+1),slice(n,n+1,list)(0))
  //(List('a, 'c, 'd),'b)
}

//v21
val v21 = insertAt('new, 1, List('a, 'b, 'c, 'd))
// res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
def insertAt(symbol: Symbol,int: Int,list: List[Symbol]) : List[Symbol] = {
//  val b1 = list.take(int):+symbol
//  val b2 = list.drop(int)
//  val ans = b1 ::: b2
//
//  ans

  (list.take(int):+symbol) ::: list.drop(int)
}

//v22
val v22 = range(4, 9)
//List[Int] = List(4, 5, 6, 7, 8, 9)
def range(x: Int, y: Int) : List[Int] ={
  (x to y ).toList
}

//v23
val v23 = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
// res0: List[Symbol] = List('e, 'd, 'a)
def randomSelect(int: Int, list: List[Symbol]) : List[Symbol] = {
  ((scala.util.Random.shuffle(0 to list.length-1)).toList).take(int).map(x => (x,list(x))).map(x => x._2)
}

//v24
val v24 = lotto(6, 49)
//res0: List[Int] = List(23, 1, 17, 33, 21, 37)
def lotto(x: Int, y: Int) : List[Int] = {
  val numberRandom = (scala.util.Random.shuffle(1 to y)).toList
  numberRandom.take(x)
}

//v25
val v25 = randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
def randomPermute(list: List[Symbol]) : List[Symbol] = {
  randomSelect(list.length,list)
}

//v26
val v26 = combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
def combinations (int: Int, list: List[Symbol]) : List[List[Symbol]] = {

  list.combinations(int).toList
  //println(list.combinations(int).toList.count())
}

////27 a
//val v27a = group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
////List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
//def group3 (list: List[String]) : List[List[String]] = {
//  ???
//}
////27 b
//val v27b = group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
////res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
//def group(list1: List[Int], list2: List[String]) : List[List[List[String]]] ={
//  ???
//}

//v28a
val v28a = sort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
def sort(list: List[List[Symbol]]) : List[List[Symbol]] = {
  list.sortWith(_.length < _.length)

  //List(List('o))
}

//v28b
val v28b = sortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
def sortFreq(list: List[List[Symbol]]) : List[List[Symbol]] = {
  list.groupBy(_.length).toList.map(_._2).sortBy(_.length).flatten
}