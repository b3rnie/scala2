name := "scala2"

version := "1.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.3.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"


// spray-can
resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "io.spray" % "spray-can" % "1.3.1"

libraryDependencies += "io.spray" % "spray-routing" % "1.3.1"

libraryDependencies += "postgresql" % "postgresql" % "9.2-1002.jdbc4"
