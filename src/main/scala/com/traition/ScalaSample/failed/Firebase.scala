//package com.traition.ScalaTutorial.manager
//
//import java.io.InputStream
//
//import com.google.firebase.database.{DatabaseReference, FirebaseDatabase}
//import com.google.firebase.{FirebaseApp, FirebaseOptions}
//
//
//case class FirebaseException(s: String) extends Exception(s)
//
//object Firebase {
//  private val credentials: InputStream = getClass.getResourceAsStream("/firebaseCredentials.json")
//  private val option = new FirebaseOptions.Builder()
//    .setDatabaseUrl("https://firebase-scala.firebaseio.com")
//    .setServiceAccount(credentials)
//    .build()
//  FirebaseApp.initializeApp(option)
//  private val database = FirebaseDatabase.getInstance()
//
//  def ref(path: String): DatabaseReference = database.getReference(path)
//}
