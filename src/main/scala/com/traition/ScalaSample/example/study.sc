import sun.misc.FloatingDecimal.BinaryToASCIIConverter

val l = List(3,3,5,2,5,2)

def f(x :Int) = if (x > 1) Some(x) else 0

l.map(x => f(x))

l.map(x => if(x>2) x else None)

l.takeRight(3)

l.sorted

def pack(list: List[Int]): List[List[Int]] = list match {
  case Nil=>Nil
  case _ => {
    val (done, left) = list.span(_ == list.head)
    (done) +: pack(left)  }
}

def lOfInt(list: List[List[Int]]) : List[Int] = {
  list.foldLeft(List[Int]()) {
    (s, i) => {
      s :+ i.length
    }
  }
}

def lOfNum(list: List[Int]) : List[Int] = {
  list.foldLeft(List[Int]()) {
    (s, i) => {
      if (s.isEmpty) List(i)
      else if(s.last == i) s
      else s :+ i
    }
  }
}

val numpali = List(1,2,3,2,1)
numpali.equals(numpali.reverse)

"%03d".format(1)


// val ans =(range.toList.filter(g=>g%2==0).filter(t=>t.isPrime==false).filter(k=>k>100).map(i=>i.goldbach)).filter(x=>(x(0)._1)>50).filter(x=>(x(0)._2)>50)