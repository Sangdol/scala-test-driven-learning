import org.scalatest.funsuite.AnyFunSuite

import scala.util._

class TryTest extends AnyFunSuite {
  test("constructors") {
    val s1 = Success(1)
    val s2 = Try(1)

    assert(s1 == s2)

    // Call by name
    val f1 = Try(1/0)
    assert(f1.isFailure)

    // call by value
    val f2 = Failure(new Exception())
    assert(f1.isFailure)
  }

  test("pattern matching") {
    val msg = Try(1/0) match {
      case Success(x) => "no way"
      case Failure(e) => e.getMessage
    }

    assert(msg == "/ by zero")
  }

  test("toEither") {
    val e = Try(1/0).toEither

    assert(e.isLeft)
  }

  test("for-comprehension") {
    val e = for {
      t1 <- Try(0)
      t2 <- Try(1/t1)
    } yield t2

    assert(e.isFailure)
  }

  test("flatten Tries") {
    val tt: Try[Try[Int]] = Try(Try(1))

    assert(tt.flatten == Try(1))
  }

  test("flatten list") {
    val s: Seq[Try[Int]] = Seq(Try(1), Try(2), Try(1/0))
    val (failures, successes) = s.partitionMap(_.toEither)

    assert(successes.sum == 3)
  }

}
