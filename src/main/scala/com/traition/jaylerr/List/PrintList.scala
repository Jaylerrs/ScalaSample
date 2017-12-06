package com.traition.jaylerr.List

object PrintList {
  def printList(list: List[Any]): Unit = {
    var int:Int = 0
    print("Print : ")
    while (int < list.length){
      print(" ".concat(list(int).toString))
      int += 1
    }
  }
}
