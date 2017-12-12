package com.traition.ScalaTutorial

import java.time

import com.traition.ScalaTutorial.Manager.ListManager

object main {
  val ls: List[Int] = List(1, 2, 3, 4)
  val adList: List[Symbol] = List('a, 'b, 'c, 'd)
  val afList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f)
  val ahList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)

  def main(args: Array[String]): Unit = {
    println(time.Year.now())
    println("getting start with SCALA")
    (ahList ++ afList ++ adList ++ ahList).foreach(x => print("-"))
    var info:List[String] = List()
    var temp:String = ""
    println("\n Please provide your information")
    do {
      print("Your first name : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length < 5 ) println("your first name is too short, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length < 5)

    do {
      print("Your last name : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length < 5 ) println("your last name is too short, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length < 5)

    do {
      print("Your nick name : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length < 1 ) println("your nick name is too short, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length < 1)

    do {
      print("Your phone number : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length != 10 ) println("your phone is invalid, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length != 10)

    do {
      print("Your birth date (example DD/MM/YYYY) : ")
      temp = scala.io.StdIn.readLine()
    } while (!isBirthDate(temp))
    info = ListManager.addItemToList(info, bd(temp))

    do {
      print("Your address : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length < 5 ) println("your address is too short, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length < 10)

    info.foreach(x=>println(x))
  }

  def bd(string: String):String={
    string.substring(0,2) ++" "++ month((string.substring(2,4)).toInt) ++" "++ setYear(string.substring(4,8).toInt)
  }

  def isBirthDate(string: String):Boolean = {
    if (string.length == 8){
      if (string.substring(0,2).toInt < 31 && string.substring(0,2).toInt >=0){
        if (string.substring(2,4).toInt >= 1 && string.substring(2,4).toInt <=12){
          val a = (setYear(string.substring(4,8).toInt)).toInt
          val b = time.Year.now().toString
          if ((b.toInt-a) >= 15){
            true
          } else {
            println("Your are is too young")
            false
          }
        } else {
          println("Invalid month")
          false
        }
      } else {
        println("Invalid date")
        false
      }
    } else {
      println("Invalid Birth date")
      false
    }
  }

  def month(int: Int): String = int match {
    case 1 => "January"
    case 2 => "February"
    case 3 => "March"
    case 4 => "April"
    case 5 => "May"
    case 6 => "June"
    case 7 => "July"
    case 8 => "August"
    case 9 => "September"
    case 10 => "October"
    case 11 => "November"
    case 12 => "December"
  }

  def setYear(int: Int):String ={
    if (int > 2017){
      (int - 543).toString
    } else int.toString
  }
}
