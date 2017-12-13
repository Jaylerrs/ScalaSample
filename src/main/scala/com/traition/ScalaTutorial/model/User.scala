package com.traition.ScalaTutorial.model
import scala.beans.BeanProperty

case class User(id:String, name: String, email:String, role:Option[String] = None, imageURL: Option[String] = None) {
  def toBean = {
    val user = new UserBean()
    user.id = id
    user.name = name
    user.email = email
    user.role = role getOrElse null
    user.imageURL = imageURL getOrElse null
    user
  }
}

class UserBean() {
  @BeanProperty var id:String = null
  @BeanProperty var name:String = null
  @BeanProperty var email:String = null
  @BeanProperty var role:String = ""
  @BeanProperty var imageURL:String = ""
  def toCase:User = {
    val maybeRole = if (role.isEmpty) None else Some(role)
    val maybeImageURL = if (imageURL.isEmpty) None else Some(imageURL)
    User(id, name, email, maybeRole, maybeImageURL)
  }
}