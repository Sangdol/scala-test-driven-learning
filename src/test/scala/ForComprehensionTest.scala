import org.scalatest.funsuite.AnyFunSuite

class ForComprehensionTest extends AnyFunSuite {

  test("for-comprehensions with lists") {
    // https://docs.scala-lang.org/tour/for-comprehensions.html
    val evens =
      for (ns <- List(1, 2, 3) if ns % 2 == 0)
        yield ns

    assert(evens == List(2))

    val evensDouble =
      for {
        ns <- List(1, 2, 3) if ns % 2 == 0
        ds = ns * 2
      } yield ds

    assert(evensDouble == List(4))

    val evensTriple =
      for {
        ns <- List(1, 2, 3) if ns % 2 == 0
      } yield ns * 3

    assert(evensTriple == List(6))
  }

  test("for-comprehension") {
    val some = Some(1)
    val some2 = Some(2)

    val s = for {
      s1 <- some
      s2 <- some2
    } yield s1 + s2

    assert(s == Some(3))
  }

}
