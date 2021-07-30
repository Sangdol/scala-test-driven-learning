name := "scala-test-driven-learning"

version := "0.1"

scalaVersion := "2.13.6"

// https://circe.github.io/circe/parsing.html
lazy val circe = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-generic-extras",
  "io.circe" %% "circe-parser"
).map(_ % "0.13.0")

// What does ++=, %, %% mean?
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
  "org.mockito" %% "mockito-scala" % "1.13.9" % Test,
  "org.mockito" %% "mockito-scala-scalatest" % "1.13.9" % Test
) ++ circe

// https://www.scala-sbt.org/1.x/docs/Howto-Scala.html
console / initialCommands := """
  import fpinscala._

  println("fpinscala is imported")
"""
