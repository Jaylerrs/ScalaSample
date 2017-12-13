package com.traition.ScalaTutorial
import java.time

import com.traition.ScalaTutorial.manager.{FileManager, ListManager}

object main {
  val ls: List[Int] = List.range(1,4)
  val adList: List[Symbol] = List('a, 'b, 'c, 'd)
  val afList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f)
  val ahList: List[Symbol] = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
  val za = "abcdefghijklmnopqrstuvwxyz".toList
  val azList: List[Char] = za

  def opening():Unit = {
    printX(33, "=")
    println(time.Instant.now())
    println("getting start with SCALA")
  }

  def printX(int: Int, string: String):Unit = {
    if (int>0){
      print(string)
      printX(int-1, string)
    } else {
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    opening()
    whatDoUWant()
  }

  def readInt():Int={
    val text = scala.io.StdIn.readLine()
    var select:(Int) = 0
    try{if (text.toInt.isValidInt) select = text.toInt else select = 0}
    catch {case _ => select = 0}
    select
  }

  def whatDoUWant(): Unit ={
    printX(33, "=")
    println(
      """press 1, Add new member
        |press 2, Print all member
        |press 3, Find member""".stripMargin)
    print("*Select : ")

    var select = readInt()

    if (select != 1001){
      while (select>3 || select <1){
        if (select>3 || select <1) {
          println("Please select only we have provided")
          print("Select : ")
          select = readInt()
        }
      }
    }

    select match {
      case 1 => inputInfo()
      case 2 => FileManager.readAllName()
      case 3 => FileManager.findOne()
      case 1001 => remove()
    }

    println()
    printX(51, "*")
    print(
      """End this? press 'EXIT'
        |Wanna continue? press any key
        |select : """.stripMargin)
    val out = scala.io.StdIn.readLine().toString
    out.toUpperCase() match {
      case "EXIT" =>
      case _ => whatDoUWant()
    }
  }

  def remove(): Unit ={
    print(
      """
        |**********************************************************
        |**  This option are risk and effect all around.         **
        |**  Please make sure you have to remove member's data.  **
        |**  This option require password to continue            **
        |**********************************************************
        |Password : """.stripMargin)
    val standardIn = System.console()
    val pass = scala.io.StdIn.readLine()
    if(pass.equals("password")){
      println("Remove all member data, Are you sure? (Y to confirm any key to cancel)")
      print("Answer : ")
      val confirm = scala.io.StdIn.readLine()
      if ("Y".equals(confirm.toUpperCase())){
        FileManager.removeFile()
      }
    } else print(
      """|
         |***************************************************
         |**  The password is incorrect,                   **
         |**  So you don't have permission to remove this  **
         |**  Finally, The data are safe and still alive   **""".stripMargin)

  }

  def printInfo(info: List[String]): Unit ={
    var i = 0
    info.foreach(x=> {
      i+=1
      if (i%3==0){
        println(x.toUpperCase())
      } else print(x.toUpperCase() + " ")
    } )
  }

  def inputInfo():Unit={
    var info:List[String] = List()
    var temp:String = ""
    println(
      """+++++++++++++++++++++++++++++++++++
        |++       Adding new member       ++
        |+++++++++++++++++++++++++++++++++++""".stripMargin)
    println("Please provide your information")
    val title:List[String] = List("MR", "MRS", "MISS")
    var test = ""
    do {
      print(
        """(example Mr. Mrs. Miss.)
          |Your title name : """.stripMargin)
      temp = scala.io.StdIn.readLine()
      test = temp.toUpperCase
      if (!title.exists(test.contains)) println("your title name is invalid, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (!title.exists(test.contains))

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
      print(
        """(example DD/MM/YYYY)
          |Your birth date : """.stripMargin)
      temp = scala.io.StdIn.readLine()
    } while (!isBirthDate(temp))
    info = ListManager.addItemToList(info, bd(temp))

    do {
      print("Your address : ")
      temp = scala.io.StdIn.readLine()
      if (temp.length < 5 ) println("your address is too short, try again . .")
      else info = ListManager.addItemToList(info, temp)
    } while (temp.length < 10)

    FileManager.writeFile(info)
  }

  def bd(string: String):String={
    string.substring(0,2) ++" "++ month((string.substring(2,4)).toInt) ++" "++ setYear(string.substring(4,8).toInt)
  }

  def isBirthDate(string: String):Boolean = {
    if (string.length == 8){
      if (string.substring(0,2).toInt <= 31 && string.substring(0,2).toInt >=0){
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
