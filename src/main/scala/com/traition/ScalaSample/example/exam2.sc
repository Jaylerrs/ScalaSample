implicit class Exam(value: Int = 0){
  def isPrime() : Boolean = {
    if (value <= 1)
      false
    else if (value == 2)
      true
    else
      !(2 to (value-1)).exists(x => value % x == 0)
  }

  def gcd(x1: Int, x2:Int) : Int =
    if (x2 == 0) x1 else gcd(x2, x1 % x2)


  def isCoPrimeTo(int: Int) : Boolean = {
    gcd(value,int) == 1
  }

  def totient() : Int = {
    ((2 to value).toList).filter(t=> t.isPrime() == true).filter(y => y < value).length
  }

  def primeFactors() : List[Int] = {
    import scala.collection.mutable
    var num1 = value
    var i = 2
    val y = value
    val ans = mutable.ListBuffer[Int]()

    while (i < y){
      if(num1%i == 0) {
        ans.append(i)
        //println(i)
        num1 = num1 / i
        i = 2
      }else{
        i = i+1
      }
    }

    ans.toList
  }



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

  def primeFactorMultiplicity() : List[(Int,Int)] = {
      (lOfNum(value.primeFactors())).zip(lOfInt(pack(value.primeFactors().sorted)))
  }

  def listPrimesinRange(range: Range) : List[Int] = {
    range.toList.foldLeft(List[Int]()){
      (s,i) =>{
        if(i.isPrime()) s :+ i
        else s
      }
    }
  }

  def goldbach() : List[(Int,Int)] = {
//    val num = listPrimesinRange(1 to value) // 2, 3, 5, 7, ..., max
//    var numMax = 0
//    var numMin = 0
//    for(i <- 0 to num.length-1){
//      for(j <- 0 to num.length -1){
//        if( num(num.length-1-i)+num(j) == value) {
//          numMax = num(num.length-1-i)
//          numMin = num(j)
//        }
//      }
//    }
//    (numMax,numMin)

    val ans =List.range(1,value).map(x=>(x,value-x)).filter(_._1.isPrime).filter(_._2.isPrime)
    List(ans(0))
  }

  def printGoldbachListLimited(range: Range, int: Int): Unit ={
//    var numMax = 0
//    var numMin = 0
//    val numReal = listPrimesinRange(1 to range.toList.last).filter(_ > int)

    val ans =(range.toList.filter(g=>g%2==0).filter(t=>t.isPrime==false).filter(k=>k>100).map(i=>i.goldbach)).filter(x=>(x(0)._1)>50).filter(x=>(x(0)._2)>50)
      .map(i=>{
        (i(0)._1+i(0)._2)+" = "+i(0)._1+"+"+i(0)._2
      })
    println(ans)

  }

  def printGoldbachList(range: Range):Any={
    val ans=range.toList.filter(g=>g%2==0).filter(t=>t.isPrime==false).map(i=>{
      val x=i.goldbach
      i+" = "+x(0)._1+"+"+x(0)._2
    })
    println(ans)
  }

}

//31
val v31 =  7.isPrime
//res0: Boolean = true

//32
val v32 = Exam().gcd(36, 63)
//  res0: Int = 9

//33
val v33 = 31.isCoPrimeTo(64)
//res0: Boolean = true

//34
val v34 = 13.totient

//35
val v35 = 315.primeFactors

//36a
val v36a = 315.primeFactorMultiplicity

//36b
val v36b = 315.primeFactorMultiplicity.toMap

//37
//val v37 =

//38

//39
val v39 = Exam().listPrimesinRange(7 to 31)

//40
val v40a = 30.goldbach


//41a
Exam().printGoldbachList(9 to 20)

//41b
Exam().printGoldbachListLimited(1 to 2000, 50)