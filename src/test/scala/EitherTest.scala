import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {
  test("for-comprehension") {
    // from Programming Scala, 2nd Edition
    def positive(i: Int): Either[String, Int] =
      if (i > 0) Right(i) else Left(s"nonpositive number $i")

    val i = for {
      i1 <- positive(5)
      i2 <- positive(2)
    } yield i1 + i2

    assert(i == Right(7))

    val j = for {
      i1 <- positive(5)
      i2 <- positive(-i1 * 3)
      i3 <- positive(-i1 * 3)
    } yield i1 + i2 + i3

    assert(j == Left("nonpositive number -15"))
  }
}
