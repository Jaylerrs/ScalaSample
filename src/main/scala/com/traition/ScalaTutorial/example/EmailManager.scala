package com.traition.ScalaTutorial.example

object EmailManager {
  def isEmail(email:String): Boolean = {
    email.contains("@") && email.contains(".") && email.length >=10
  }
}
