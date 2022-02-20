package com.iamsang.future

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util._

/**
  * No `main` is needed with `App`.
  * https://www.scala-lang.org/api/2.13.3/scala/App.html
  */
object FutureMain extends App {
  println("00: hallo")

  val ff = Future.failed(new RuntimeException())
  val f1 = Future { println("01"); 1 }
  val f2 = Future { println("02"); 2 }

  Future.sequence(Seq(f1, f2)).foreach {
    case s => println(s"10: $s")
  }

  Future.sequence(Seq(f1, f2, ff)).foreach {
    case s => println("this won't be printed.")
  }

  println(20)

  // This runs all futures but throws an exception.
  //Await.result(Future.sequence(Seq(ff, f1, f2)), 1.second)

  println("99: bis bald")

}
