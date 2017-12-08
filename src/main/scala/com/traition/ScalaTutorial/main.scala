package com.traition.ScalaTutorial

object main {

  import scala.math._
  val ls: List[Int] = List(1, 1, 2, 3, 5, 8)
  val adList: List[Symbol] = List('a, 'b, 'c, 'd)
  val afList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f)
  val ahList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)

  def main(args: Array[String]): Unit = {
    println("Welcome let's starting SCALA")

    val l1 = ListManager.addItemToList(ls, 9)
    ListManager.printList(l1)
    println(ListManager.isPalindrome(l1))
    println(ListManager.flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

    val a = (x:Int) => x*5
    println(a(5))
    val userDir = () => { System.getProperty("user.dir") }
    println(userDir())

    val b = (xx:Int) => xx*a(xx)
    println(b(5))

    val c = (xxx:String) => xxx + b(5).toString()
    println(c("Can I borrow your money only for ") + " dollar")
    println(c("Please give me ") + (10+3*5/2).toString + " dollar")
    println(round(10.4))
    val age = 23
    val canVote = if (age >=18) "can" else "can not"
    val canVote2 = if (age>=18) "Yes," else "No,"
    println(canVote2+" I " + canVote + " vote")
  }
}
