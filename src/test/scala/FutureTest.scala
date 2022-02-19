import org.scalatest.funsuite.AsyncFunSuite

import scala.concurrent.Future
import scala.util._

/**
  * Futures and Promises
  * https://docs.scala-lang.org/overviews/core/futures.html
  *
  * Scala 3 colorful spec documents
  * - trait Future https://dotty.epfl.ch/api/scala/concurrent/Future.html
  * - object Future https://dotty.epfl.ch/api/scala/concurrent/Future$.html
  */
class FutureTest extends AsyncFunSuite {

  test("foreach") {
    val ff = Future(1)

    ff.foreach {
      // This test wouldn't fail even if this assert thorws an exception
      // since this runs in another thread.
      case n => assert(n == 1)
    }

    // This is needed to return Future[Assertion]
    // since foreach returns Unit.
    ff map { n => assert(n == 1) }
  }

  test("andThen") {
    val f = Future { 5 }

    f andThen {
      case r => r
    } map { r =>
      assert(r == 5)
    }

    f andThen {
      case Success(r) => r
    } map { r => assert(r == 5) }

    f andThen {
      // An exception in the chain is not propagated.
      // Not throwing exception here to remove exception log.
      //case r => throw new RuntimeException("hallo")
      case Success(r) => r + 1
    } andThen {
      case Failure(f) => -1
      case Success(s) => s
    } map { r => assert(r == 6) }

    val pf: PartialFunction[Try[Int], Int] = {
      case Failure(_) => 10
      case Success(s) => s
    }

    Future { 5 } andThen pf map { n =>
      assert(n == 5)
    }
  }

  test("onComplete") {
    val f = Future { 5 }

    // This test wouldn't fail even if this assert thorws an exception
    // since this runs in another thread.
    f onComplete {
      case Failure(f) => fail()
      case Success(s) => assert(s == 5)
    }

    val ft: (Try[Int] => Unit) = {
      case Failure(f) => fail()
      case Success(s) => assert(s == 5)
    }

    f onComplete ft

    // This is needed to return Future[Assertion]
    // since onComplete returns Unit.
    f map { n => assert(n == 5) }
  }

  test("collect") {
    Future { 5 } collect {
      case n => n * 2
    } map { n =>
      assert(n == 10)
    }

    recoverToSucceededIf[NoSuchElementException] {
      Future { 5 } collect {
        case x if x > 5 => -x
      }
    }
  }

  test("failed") {
    val f: Future[Throwable] = Future { throw new Exception("hallo") }.failed
    f map { e => assert(e.getMessage == "hallo") }

    // Compare this to above
    recoverToSucceededIf[Exception] {
      Future { throw new Exception }
    }

    // `failed` throws NoSuchElementException
    // if the original future is successful.
    recoverToSucceededIf[NoSuchElementException] {
      Future(1).failed
    }
  }

  test("fallbackTo") {
    val f = Future { throw new RuntimeException("hallo") }
    val g = Future { 5 }

    f fallbackTo g map { n =>
      assert(n == 5)
    }

    // `fallbackTo` fallback to the first exception
    // if the second Future throws an exception.
    val h = Future { throw new Exception("hallo2") }
    recoverToSucceededIf[RuntimeException] {
      f fallbackTo h
    }
  }

  test("filter") {
    val f = Future { 5 }

    f filter { _ % 2 == 1 } map { n =>
      assert(n == 5)
    }

    // `filter` throws NoSuchElementException
    // if the original Future doesn't hold a value
    // that satisfies the predicate.
    recoverToSucceededIf[NoSuchElementException] {
      f filter { _ % 2 == 0 }
    }
  }

  test("basic for comprehension") {
    val f = for { r1 <- Future(3) } yield r1

    f map { n => assert(n == 3) }

    // the above for-comprehension is equal to
    val ff = Future(3).flatMap(Future(_))

    ff map { n => assert(n == 3) }
  }

  /** https://stackoverflow.com/questions/19045936/scalas-for-comprehension-with-futures
    */
  test("for comprehension with futures") {

    /** This runs sequentially.
      */
    val fsum1 = for {
      r1 <- Future(1)
      r2 <- Future(r1 * 2)
    } yield r1 + r2

    fsum1 map { f => assert(f == 3) }

    // the above for-comprehension is equal to
    val fsum11 = Future(1).flatMap(r1 => Future(r1 * 2).map(r2 => r1 + r2))
    fsum11 map { f => assert(f == 3) }

    /** Parallel solution
      */
    val f1 = Future(1)
    val f2 = Future(2)
    val f3 = Future(3)

    val fsum2: Future[Int] = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield r1 + r2 + r3

    fsum2 map { f => assert(f == 6) }

    // the above for-comprehension is equal to
    val fsum21 = Future(1).flatMap(r1 => Future(2).flatMap(r2 => Future(3).map(r3 => r1 + r2 + r3)))
    fsum21 map { f => assert(f == 6) }
  }

  test("conditional for comprehension") {

    def condF1(i: Int): Future[Int] = {
      for {
        r1 <- Future(i)
        r2 <- if (r1 > 0) Future(2) else Future(0)
      } yield r1 + r2
    }

    condF1(1) map { c => assert(c == 3) }
    condF1(0) map { c => assert(c == 0) }

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

    condF2(0) map { c => assert(c == 3) }
    condF2(1) map { c => assert(c == 6) }

    def condF3(i: Int): Future[Int] = {
      val f = Future(i)

      for {
        r1 <- Future(i)
        r2 <- if (r1 > 0) Future(2) else Future(0)
        r3 <- Future(3)
      } yield r1 + r2 + r3
    }

    condF3(0) map { c => assert(c == 3) }
    condF3(1) map { c => assert(c == 6) }
  }

  test("sequence and recover") {
    val f1 = Future(1)
    val f2 = Future(2)
    val fs = Future.sequence(Seq(f1, f2))

    fs map { f => assert(f.sum == 3) }

    val ff = Future.failed(new Exception())
    val fs2 = Future.sequence(Seq(f1, ff))

    // similar to assertThrows
    recoverToSucceededIf[Exception] {
      fs2
    }

    val fr1 = fs2.recover {
      case e: Exception =>
        0
    }

    fr1 map { f => assert(f == 0) }

    val fr2 = fs2.recoverWith {
      case e: Exception =>
        Future(-1)
    }

    fr2 map { f => assert(f == -1) }
  }

  test("sequence and failure separation 1") {
    val f1 = Future(1)
    val f2 = Future(2)
    val ff = Future.failed(new Exception())

    // transform(f: Try[T] => Try[S])
    val futures: Seq[Future[Try[Int]]] =
      Seq(f1, f2, ff).map(_.transform(Try(_)))

    val sum = Future
      .sequence(futures)
      .map { tries =>
        val (failures, successes) = tries.partitionMap {
          case Success(n) => Right(n)
          case Failure(n) => Left(n)
        }

        val fsum = failures.map(_ => 100).sum
        val ssum = successes.sum

        fsum + ssum
      }

    sum map { s => assert(s == 103) }
  }

  test("sequence and failure separation 2") {
    val f1 = Future(1)
    val f2 = Future(2)
    val ff = Future.failed(new Exception())

    // transform(f: Try[T] => Try[S])
    val futures: Seq[Future[Either[Throwable, Int]]] =
      Seq(f1, f2, ff).map(_.transform(f => Success(f.toEither)))

    val sum = Future
      .sequence(futures)
      .map { eithers =>
        val (failures, successes) = eithers.partitionMap(identity)

        val fsum = failures.map(_ => 100).sum
        val ssum = successes.sum

        fsum + ssum
      }

    sum map { s => assert(s == 103) }
  }

  // https://stackoverflow.com/questions/20874186/scala-listfuture-to-futurelist-disregarding-failed-futures
  test("sequence and failure separation 3") {
    val f1 = Future(1)
    val f2 = Future(2)
    val ff = Future.failed(new Exception())

    // transform(f: Try[T] => Try[S])
    val futures: Seq[Future[Try[Int]]] =
      Seq(f1, f2, ff).map(_.transform(Try(_)))
    val futureTries: Future[Seq[Try[Int]]] = Future.sequence(futures)

    // It's not very easy to flatten Tries
    // https://stackoverflow.com/questions/15495678/flatten-scala-try
    val failureTries: Future[Seq[Try[Int]]] =
      futureTries.map(_.filter(_.isFailure))
    val successeTries: Future[Seq[Try[Int]]] =
      futureTries.map(_.filter(_.isSuccess))

    // _.collect() to remove Try
    val failures: Future[Seq[Throwable]] = futureTries.map(_.collect {
      case Failure(x) => x
    })
    val successes: Future[Seq[Int]] = futureTries.map(_.collect {
      case Success(x) => x
    })

    successes map { fs => assert(fs.sum == 3) }
  }

}
