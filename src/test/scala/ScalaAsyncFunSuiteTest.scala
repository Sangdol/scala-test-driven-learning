import org.scalatest.funsuite.AsyncFunSuite

import scala.concurrent.Future

/**
  * Asynchronous testing
  * - https://www.scalatest.org/user_guide/async_testing
  */
class ScalaAsyncFunSuiteTest extends AsyncFunSuite {

  test("map assert") {
    Future { 1 } map { n => assert(n == 1) }
  }

  test("recoverToSucceededIf / assertThrows") {
    recoverToSucceededIf[Exception] {
      Future { throw new Exception }
    }

    recoverToSucceededIf[NoSuchElementException] {
      Future { 5 } collect {
        case x if x > 5 => -x
      }
    }
  }

  test("recoverToExceptionIF") {
    // returns Future[IllegalStateException] instead of Future[Assertion]
    val ex = recoverToExceptionIf[IllegalStateException] {
      Future { throw new IllegalStateException("hallo") }
    }

    ex map { e => assert(e.getMessage == "hallo") }
  }

}
