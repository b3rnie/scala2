import sbt._

object MyBuild extends Build {
  lazy val root = Project("root", file("."))
}
