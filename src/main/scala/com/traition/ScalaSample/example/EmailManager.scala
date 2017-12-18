package com.traition.ScalaSample.example

object EmailManager {
  def isEmail(email:String): Boolean = {
    email.contains("@") && email.contains(".") && email.length >=10
  }
}
