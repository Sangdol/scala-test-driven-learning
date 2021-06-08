name := "scala-test-driven-learning"

version := "0.1"

scalaVersion := "2.13.6"

// What does ++=, %, %% mean?
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
)

// https://www.scala-sbt.org/1.x/docs/Howto-Scala.html
console / initialCommands := """
  import fpinscala._

  println("fpinscala is imported")
"""
