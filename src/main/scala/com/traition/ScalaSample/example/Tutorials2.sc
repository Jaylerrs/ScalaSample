import scala.collection.mutable.ListBuffer

//add list
def addItemToList[A](list: List[A], any: A): List[A] = {
  val newList  = list :+ any
  newList
}

def printList(list: List[Any]): Unit = {
  var int:Int = 0
  print("Print : ")
  while (int < list.length){
    print(" ".concat(list(int).toString))
    int += 1
  }
}

///////////////////////////////////////////////////////////
/////
//val ex31 = xxxxxx
//class S99Int(val start: Int) {
//  val primes: List[Int] = listPrimesInRange(1, start)
//  def isPrime: Boolean =
//    (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })
//}
//
//object S99Int {
//  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
//}

/////
val ex32 = gcd(35, 64)
def gcd(m: Int, n: Int): Int =
  if (n == 0) m else gcd(n, m % n)

def Old_gcd(a:Int, b:Int): Int = {
  var result: Boolean = true
  var x:Int = 0
  var y:Int = 0
  var  i:Int = 1
  if (b>a){
    x = a
    y = b
  } else {
    x = b
    y = a
  }
  var z:Int = 0
  while (result){
    z = x/i
    println("Y%Z = "+y%z)
    if (y%z == 0){
      result = false
    } else if (i >= x/2){
      result = false
      z = 0
    }
    i+=1
  }
  print("GCD of two positive integer numbers. : ")
  if (z == 0){
    print(" Nothing is GCD for this ")
    z
  } else z
}

/////
val intManager = new IntManager(35)
val ex33 = intManager.isCoPrimeTo(64)
val intManager2 = new IntManager(10)
val ex34 = intManager2.toTient()
class IntManager(val start: Int) {
  import MyFunc._
  ///33
  def isCoPrimeTo(n: Int): Boolean = {
    gcd(start, n) == 1
  }

  //34
  def toTient(): Int ={
    var i:Int = 0
    var n:Int = 0
    while (i < start){
      i+=1
      if (isCoPrimeTo(i)){
        n+=1
      }
    }
    n
  }

  //
  def isPrime(int: Int):Boolean = {
    var result:Boolean = false
    if (int < 2){
      result = false
    } else if (int == 2){
      result = true
    } else if (int % 2 == 0){
      result = false
    } else {
      var i:Int = 3
      var br:Boolean = true
      while (i*i <= int){
        if (int % i == 0){
          result = false
          br = false
        }
        i+=2
      }
      if (br){
        result = true
      } else result = false
    }
    result
  }


  def goldBach(): List[Int] = {
    val primes: List[Int] = listPrimesInRange(1,start)
    primes takeWhile { _ < start } find { p => isPrime((start - p)) } match {
      case None     => List(0,0)
      case Some(p1) => List(p1, start - p1)
    }
  }
}
//35

//36

//37

//38

//39
import MyFunc._
val ex39 = listPrimesInRange(7, 31)
object MyFunc {
  def listPrimesInRange(m: Int, n: Int): List[Int] = {
    require(n >= 2)

    val primes = ListBuffer(2)
    for (i <- 3 to n) {
      if (prime(i, primes.iterator)) {
        primes += i
      }
    }
    val p = primes.filter(_ > m)
    p.toList
  }
}
// factors must be in sorted order
def prime(num: Int, factors: Iterator[Int]): Boolean =
  factors.takeWhile(_ <= math.sqrt(num).toInt) forall(num % _ != 0)

/////
val intManager3 = new IntManager(28)
val ex41 = intManager3.goldBach()
def printGoldBachListAll(map:Map[Int, String]): Unit ={
  println("Printing : " + map.size + " value ")
  map foreach {case (key, value) => println (key + " = " + value)}
}

/////
val ex41_1 = printGoldBachList(9 to 20)


def printGoldBachList(r:Range): Map[Int, String] = {
  var i:Int = 0
  if (r.start % 2 == 0){
    i = r.start
  } else i=r.start+1
  var gbl:Map[Int, String] = Map()
  var finalList:List[Any] = List()
  while (i < r.end){
    var im = new IntManager(i)
    var lgb:List[Int] = List()
    lgb = im.goldBach()
    if(lgb(0) != 0){
      finalList = addItemToList(finalList, (i, lgb))
      gbl = gbl ++ Map(i -> (lgb(0) + " + " + lgb(1)))
    }
    i+=2
  }
  printGoldBachListAll(gbl)
  gbl
}

/////
val ex41_2 = printGoldBachListLimited(1 to 2000, 50)
def printGoldBachListLimited(range: Range, int: Int): Map[Int, String] = {
  var i:Int = 0
  if (range.start % 2 == 0){
    i = range.start
  } else i=range.start+1
  var gbl:Map[Int, String] = Map()
  var finalList:List[Any] = List()
  while (i < range.end){
    var im = new IntManager(i)
    var lgb:List[Int] = List()
    lgb = im.goldBach()
    if(lgb(0) > 50){
      finalList = addItemToList(finalList, (i, lgb))
      gbl = gbl ++ Map(i -> (lgb(0) + " + " + lgb(1)))
    }
    i+=2
  }
  printGoldBachListAll(gbl)
  gbl
}

/////
val logical = new Logical()
val ex46= logical.equ(true, true)
val ex46_2 = logical.xor(true, true)
val a,b = true
val ex46_3 = logical.and(a, logical.or(a, b) )
type B = Boolean

class Logical(){
  def and(x:B, y:B): B = x && y
  def or(x:B, y:B): B = x || y
  def nAnd(x:B, y:B): B = !(and(x,y))
  def nor(x:B, y:B): B = !(or(x,y))
  def xor(x:B, y:B): B = !(equ(x,y))
  def impl(x:B, y:B): B = or(!x, y)
  def equ(x:B, y:B): B = or(and(x, y), and(!(x), !(y)))
}

/////
val ex46_4 = table2((a: Boolean, b: Boolean) => logical.and(a, logical.or(a, b)))
def table2(f: (B, B) => B) {
  println("A     B     result")
  for {a <- List(true, false);
       b <- List(true, false)} {
    printf("%-5s %-5s %-5s\n", a, b, f(a, b))
  }
}
/////
val ex49 = P49.gray(3)
def gray(int: Int):String = {
  var  i:Int = 0
  while (i < int){

  }
  null
}


object P49 {
  def gray(n: Int): List[String] =
    if (n == 0) List("")
    else {
      val lower = gray(n - 1)
      (lower map {
        "0" + _
      }) ::: (lower.reverse map {
        "1" + _
      })
    }

  import scala.collection.mutable

  private val strings = mutable.Map(0 -> List(""))

  def grayMemorized(n: Int): List[String] = {
    if (!strings.contains(n)) {
      strings + (n -> ((grayMemorized(n - 1) map {
        "0" + _
      }) :::
        (grayMemorized(n - 1).reverse map {
          "1" + _
        })))
    }
    strings(n)
  }
}

def egc(x:Int, y:Int): Int ={
  x+y
}
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////