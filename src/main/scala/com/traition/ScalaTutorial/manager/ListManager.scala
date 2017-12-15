package com.traition.ScalaTutorial.manager

object ListManager {
  def printList(list: List[Any]): Unit = {
    var int:Int = 0
    print("=>")
    while (int < list.length){
      print(" ".concat(list(int).toString))
      int += 1
    }
    println
  }

  //add list
  def addItemToList[A](list: List[A], any: A): List[A] = {
    val newList  = list :+ any
    newList
  }

  def last(list: List[Int]): Int = list(list.length - 1)

  def penultimate(list: List[Int]): Int = list(list.length - 2)

  def nth(item: Any, list: List[Any]): Any = {
    var length:Int = 0
    while(list(length) != item){
      length+=1
    }
    list(length)
  }

  def lengthOfList(list: List[Int]): Int = list.length

  def reverseList(list: List[Int]): List[Any] = {
    var  listR:List[Int] = List()
    listR = list.reverse
    listR
  }

  //Palindrome is a list that left and right of the list is the same
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

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (a: Int) :: tail => a :: flatten(tail)
    case (a: List[Any]) :: tail => flatten(a) ::: flatten(tail)
    case _ :: tail => flatten(tail)
  }

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

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}
