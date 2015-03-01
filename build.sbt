name := "tracker"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation"
)

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Spray"               at "http://repo.spray.io"
)

// akka
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

// logging
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

// spray-can
libraryDependencies += "io.spray" %% "spray-can" % "1.3.2"
libraryDependencies += "io.spray" %% "spray-routing" % "1.3.2"

// mysql
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.34"

// slick
libraryDependencies += "com.typesafe.slick" %% "slick" % "2.1.0"

// test
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
