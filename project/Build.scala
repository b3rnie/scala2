import sbt._

object MyBuild extends Build {
  lazy val root = Project("root", file("."))
    //.dependsOn(RootProject(uri("git://github.com/alvinj/SoundFilePlayer.git")))
}
