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

lazy val akkaHttpVersion = "10.1.11"
lazy val akkaVersion = "2.6.4"
lazy val akkaHttpJsonVersion = "1.32.0"
lazy val akka = Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "de.heikoseeberger" %% "akka-http-circe" % akkaHttpJsonVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
)

// What does ++=, %, %% mean?
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.mockito" %% "mockito-scala" % "1.13.9" % Test,
  "org.mockito" %% "mockito-scala-scalatest" % "1.13.9" % Test,

  // Property-based testing
  // https://scalacheck.org/
  // https://www.scalatest.org/user_guide/property_based_testing
  "org.scalacheck" %% "scalacheck" % "1.15.3" % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test"
) ++ circe ++ akka

// https://www.scala-sbt.org/1.x/docs/Howto-Scala.html
console / initialCommands := """
  println()
  println("===========================")
  println("Starting initialCommands...")
  println("===========================")
  println()

  import com.iamsang.future._
"""

/**
 * https://www.scala-sbt.org/1.x/docs/Process.html
 */

import scala.sys.process._

// Definition: constructing a new slot
val gitHeadCommitSha = taskKey[String]("Git commmit SHA")

// Setting: constructing a function that will compute the value
//
gitHeadCommitSha := Process("git rev-parse HEAD").lineStream.head
