import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FutureTest extends AnyFunSuite {

  /**
    * https://stackoverflow.com/questions/19045936/scalas-for-comprehension-with-futures
    */
  test("basic for comprehension") {
    val f = for {
      r1 <- Future(3)
    } yield r1

    assert(Await.result(f, 1.second) == 3)

    // the above for-comprehension is equal to
    val ff = Future(1).flatMap(Future(_))
    assert(Await.result(ff, 1.second) == 3)
  }

  test("for comprehension with futures") {

    /**
      * This runs sequentially.
      */
    val fsum1 = for {
      r1 <- Future(1)
      r2 <- Future(r1 * 2)
    } yield r1 + r2

    assert(Await.result(fsum1, 1.second) == 3)

    // the above for-comprehension is equal to
    val fsum11 = Future(1).flatMap(r1 => Future(r1 * 2).map(r2 => r1 + r2))
    assert(Await.result(fsum11, 1.second) == 3)

    /**
      * Parallel solution
      */
    val f1 = Future(1)
    val f2 = Future(2)
    val f3 = Future(3)

    val fsum2: Future[Int] = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield r1 + r2 + r3

    val sum = Await.result(fsum2, 1.second)

    // why this doesn't work?
    // https://stackoverflow.com/questions/15104536/how-does-20-seconds-work-in-scala
    //    val sum = Await.result(fsum, 1 second)

    assert(sum == 6)
  }

}
