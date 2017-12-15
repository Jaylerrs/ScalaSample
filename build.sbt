organization := "Tradition"

name := "jaylerr"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= List( "org.scalatest" %% "scalatest" % "3.0.4",
    "com.typesafe" % "slick_2.10.0-M7" % "0.11.1"
    ,"com.h2database" % "h2" % "1.3.166"
    ,"org.xerial" % "sqlite-jdbc" % "3.6.20"
    ,"org.slf4j" % "slf4j-nop" % "1.6.4" // <- disables logging
    /*
    // enables logging
      ,"org.slf4j" % "slf4j-api" % "1.6.4"
      ,"ch.qos.logback" % "logback-classic" % "0.9.28"
    */
    /*
    // Other database drivers
      "org.apache.derby" % "derby" % "10.6.1.0",
      "org.hsqldb" % "hsqldb" % "2.0.0",
      "postgresql" % "postgresql" % "8.4-701.jdbc4",
      "mysql" % "mysql-connector-java" % "5.1.13"
    */
  )


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

//libraryDependencies ++= Seq(
//  "com.typesafe.slick" %% "slick" % "3.0.0",
//  "org.slf4j" % "slf4j-nop" % "1.6.4"
//)