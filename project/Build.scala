import sbt._
import Keys._

object QuasiQuotesBuild extends Build {

  // sbt chooses default project according to lexographical
  // order, so prefix before _tests make it a default

  lazy val _tests = Project(id = "tests",
			   base = file("tests")).dependsOn(quasiquotes)

  lazy val quasiquotes = Project(id = "quasiquotes",
				   base = file("quasiquotes"))

  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.10.0-RC1",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.0-RC1"
    )
  )
}
