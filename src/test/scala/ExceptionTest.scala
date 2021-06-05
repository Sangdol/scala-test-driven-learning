import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * Exceptions are less used in Scala.
  * No checked exception in Scala.
  */
class ExceptionTest extends AnyFunSuite {
  test("try catch final") {
    var n = 2

    try {
      n / 0
    } catch {
      case NonFatal(ex) =>
        assert(ex.toString == "java.lang.ArithmeticException: / by zero")
    } finally {
      n = n * 2
    }

    assert(n == 4)

    n = 2

    try {
      n / 0
    } catch {
      case ex: ArithmeticException =>
        assert(ex.toString == "java.lang.ArithmeticException: / by zero")
    } finally {
      n = n * 2
    }

    assert(n == 4)
  }

  test("Option") {
    def toInt(s: String): Option[Int] = {
      try {
        Some(Integer.parseInt(s.trim))
      } catch {
        case e: Exception => None
      }
    }

    assert(toInt("1").contains(1))
    assert(toInt("A").isEmpty)
  }

  test("Try, Success, Failure") {
    // https://docs.scala-lang.org/overviews/scala-book/functional-error-handling.html
    def toInt(s: String): Try[Int] = Try(Integer.parseInt(s.trim))

    assert(toInt("1") == Success(1))
    assert(toInt("1").isSuccess)
    assert(toInt("a").isFailure)
  }
}
