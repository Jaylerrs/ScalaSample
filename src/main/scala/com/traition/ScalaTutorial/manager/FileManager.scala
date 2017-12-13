package com.traition.ScalaTutorial.manager
import java.io.{File, FileOutputStream, PrintWriter}

import scala.io._
object FileManager {
  def writeFile(list: List[String]):Unit ={
    val writer = new PrintWriter(new FileOutputStream(new File("info.txt"),true))
    try{
      list.foreach(x => writer.append(x + ", "))
    }
    catch {
      case _ => println("Something wrong")
    }
    finally {
      writer.append("\n")
      writer.close()
      println("Write file successfully")
    }

    readAllName()
  }

  def removeFile(): Unit ={
    val writer = new PrintWriter("info.txt")
    writer.write("")
  }

  def readAllName(): Unit ={
    println(
      """------------------------------------------
        |--       Printing all member name       --
        |------------------------------------------""".stripMargin)
    val file = Source.fromFile("info.txt").getLines()
    file.foreach(x => {
      val person = x.split(",")
      println(person(1) + " " + person(2))
    })
  }

  def findOne():Unit = {
    println(
      """------------------------------------------
        |--       Finding member information     --
        |------------------------------------------""".stripMargin)
    println("If you want to exit finding press EXIT")
    var exit = ""
    while (exit.toUpperCase() != "EXIT"){
      print("Key word : ")
      var find = scala.io.StdIn.readLine().toString
      while (find.contains(",")) {
        println(s"Key word $find can not include of symbol (example , .)")
        print("Key word : ")
        find = scala.io.StdIn.readLine().toString
      }

      val file = Source.fromFile("info.txt").getLines()
      file.foreach(x=>{
        if (x.contains(find)) ListManager.printList(List(x))
      })
      exit = find
    }
  }
}
