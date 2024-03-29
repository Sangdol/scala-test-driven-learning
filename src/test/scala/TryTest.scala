import org.scalatest.funsuite.AnyFunSuite

import scala.util._

// Try is analogous to Either with Success and Failure
// but Failure always holds a Throwable.
class TryTest extends AnyFunSuite {
  test("constructors") {
    val s1 = Success(1)
    val s2 = Try(1)

    assert(s1 == s2)

    // Call by name
    val f1 = Try(1 / 0)
    assert(f1.isFailure)

    // call by value
    val f2 = Failure(new Exception())
    assert(f1.isFailure)
  }

  test("pattern matching") {
    val msg = Try(1 / 0) match {
      case Success(x) => "no way"
      case Failure(e) => e.getMessage
    }

    assert(msg == "/ by zero")

    // exception vs. throw exception
    Try(new Throwable("no throw")) match {
      case Success(e) => assert(e.getMessage == "no throw")
      case Failure(e) => fail()
    }
    Try(throw new Throwable("throw")) match {
      case Success(e) => fail()
      case Failure(e) => assert(e.getMessage == "throw")
    }
  }

  test("try argument function") {
    val f: (Try[Int] => String) = {
      case Success(n) => n.toString
      case Failure(n) => "Fail"
    }

    assert(f(Success(1)) == "1")
    assert(f(Failure(new Exception())) == "Fail")
  }

  test("toEither") {
    val e = Try(1 / 0).toEither

    assert(e.isLeft)
  }

  test("for-comprehension simple failure") {
    val e = for {
      t1 <- Try(0)
      t2 <- Try(1 / t1)
    } yield t2

    assert(e.isFailure)
  }

  test("for-comprehension") {
    def positive(i: Int): Try[Int] =
      Try {
        assert(i > 0, s"nonpositive number $i")
        i
      }

    val s = for {
      i1 <- positive(2)
      i2 <- positive(i1 * 2)
    } yield i1 + i2

    assert(s == Success(6))

    val s2 = for {
      i1 <- positive(-2)
      i2 <- positive(i1 * 2)
    } yield i1 + i2

    assert(s2.isFailure)

    val e = s2 match {
      case Failure(e) => e
    }
    assert(e.getClass.getSimpleName == "TestFailedException")
  }

  test("flatten Tries") {
    val tt: Try[Try[Int]] = Try(Try(1))

    assert(tt.flatten == Try(1))
  }

  test("flatten list") {
    val s: Seq[Try[Int]] = Seq(Try(1), Try(2), Try(1 / 0))
    val (failures, successes) = s.partitionMap(_.toEither)

    assert(successes.sum == 3)
  }

}
