package com.traition.ScalaSample.manager
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
    val file = Source.fromFile("info.txt").getLines()
    if ((file.toString()) == "empty iterator"){
      println(
        """
          |   **********************************************
          |   **  No existing member data to remove.      **
          |   **  Don't worry, you can add new easily.    **
          |   **********************************************""".stripMargin)
    } else {
      val writer = new PrintWriter("info.txt")
      writer.write("")
      println(
        """
          |   ********************************************
          |   **  All member data are now gone.         **
          |   **  Don't worry, you can add new easily.  **
          |   ********************************************""".stripMargin)
    }
  }

  def readAllName(): Unit ={
    println(
      """   ------------------------------------------
        |   --       Printing all member name       --
        |   ------------------------------------------""".stripMargin)
    val file = Source.fromFile("info.txt").getLines()
    print("   "+file)
    file.foreach(x => {
      val person = x.split(",")
      println(person(1) + " " + person(2))
    })
  }

  def findOne():Unit = {
    println(
      """   ------------------------------------------
        |   --       Finding member information     --
        |   ------------------------------------------""".stripMargin)
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
      println("-- result ----------------------------------------")
      var i:Int = 0
      file.foreach(x=>{
        if ((x.toUpperCase()).contains(find.toUpperCase())) {
          ListManager.printList(List(x))
          i+=1
        }
      })
      if (i<1){
        println("     not found")
      }
      exit = find
    }
  }
}
