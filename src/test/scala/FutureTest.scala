import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Doc https://docs.scala-lang.org/overviews/core/futures.html
  */
class FutureTest extends AnyFunSuite {

  test("basic for comprehension") {
    val f = for {
      r1 <- Future(3)
    } yield r1

    assert(Await.result(f, 1.second) == 3)

    // the above for-comprehension is equal to
    val ff = Future(3).flatMap(Future(_))
    assert(Await.result(ff, 1.second) == 3)
  }

  /**
    * https://stackoverflow.com/questions/19045936/scalas-for-comprehension-with-futures
    */
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

    // the above for-comprehension is equal to
    val fsum21 = Future(1).flatMap(r1 => Future(2).flatMap(r2 => Future(3).map(r3 => r1 + r2 + r3)))
    assert(Await.result(fsum21, 1.second) == 6)
  }

  test("conditional for comprehension") {

    def condF1(i: Int): Future[Int] = {
      for {
        r1 <- Future(i)
        r2 <- if (r1 > 0) Future(2) else Future(0)
      } yield r1 + r2
    }

    assert(Await.result(condF1(1), 1.second) == 3)
    assert(Await.result(condF1(0), 1.second) == 0)

    // It seems it's not possible to write this with one for comprehension
    // => did it with an additional Future(0) - see condF3.
    def condF2(i: Int): Future[Int] = {
      Future(i).flatMap(r1 =>
        if (r1 > 0)
          Future(2).flatMap(r2 => Future(3).map(r3 => r1 + r2 + r3))
        else
          Future(3).map(r3 => r1 + r3)
        )
    }

    assert(Await.result(condF2(0), 1.second) == 3)
    assert(Await.result(condF2(1), 1.second) == 6)

    def condF3(i: Int): Future[Int] = {
      val f = Future(i)

      for {
        r1 <- Future(i)
        r2 <- if (r1 >0) Future(2) else Future(0)
        r3 <- Future(3)
      } yield r1 + r2 + r3
    }

    assert(Await.result(condF3(0), 1.second) == 3)
    assert(Await.result(condF3(1), 1.second) == 6)
  }

  test("sequence and recover") {
    val f1 = Future(1)
    val f2 = Future(2)
    val fs = Future.sequence(Seq(f1, f2))

    assert(Await.result(fs, 1.second).sum == 3)

    val ff = Future.failed(new Exception())
    val fs2 = Future.sequence(Seq(f1, ff))

    assertThrows[Exception](Await.result(fs2, 1.second))

    val fr1 = fs2.recover {
      case e: Exception => 0
    }

    assert(Await.result(fr1, 1.second) == 0)

    val fr2 = fs2.recoverWith {
      case e: Exception => Future(-1)
    }
    assert(Await.result(fr2, 1.second) == -1)
  }

}
