name := "jaylerr"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4"

//"com.47deg" %% "fetch" % "0.6.0"

//lazy val root = (project in file("."))
//  .settings(
//    name := "interactive-classroom-server",
//    version := "1.0",
//    scalaVersion := "2.12.4",
//    libraryDependencies ++= Seq(
//      "com.google.firebase" % "firebase-server-sdk" % "5.6.0"
//    )
//  )

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4"
)