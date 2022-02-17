package com.iamsang.future

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util._

/**
  * No `main` is needed with `App`.
  * https://www.scala-lang.org/api/2.13.3/scala/App.html
  */
object FutureMain {

  def printAndThrowException(): Unit = {
    println("hallo")

    val ff = Future.failed(new Exception())
    val f1 = Future { println(1); 1 }
    val f2 = Future { println(2); 2 }

    // This runs all futures but throws an exception.
    Await.result(Future.sequence(Seq(ff, f1, f2)), 1.second)

    println("bis bald")
  }

}
