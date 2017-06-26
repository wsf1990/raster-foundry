import sbt._

object BuildWithContrib extends Build {
  lazy val adHocLabsAkkaHttpContrib = RootProject(uri("git://github.com/adhoclabs/akka-http-contrib.git"))
}
