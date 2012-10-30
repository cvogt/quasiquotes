scalaVersion := "2.10.0-RC1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0-RC1",
  "org.scalatest" % "scalatest_2.10.0-RC1" % "1.8" % "test"
)