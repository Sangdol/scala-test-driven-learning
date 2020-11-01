name := "scala-test-driven-learning"

version := "0.1"

scalaVersion := "2.13.1"

// What does ++=, %, %% mean?
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)

// https://www.scala-sbt.org/1.x/docs/Howto-Scala.html
initialCommands in console := """
  import fpinscala._

  println("fpinscala is imported")
"""
