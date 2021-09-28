import org.scalatest.funsuite.AnyFunSuite

class TupleTest extends AnyFunSuite {

  test("Tuples") {
    val tuples = ("Sang", 38, 0.1)

    assert(tuples._1 == "Sang")
    assert(tuples._2 == 38)

    val (name, age, _) = tuples

    assert(name == "Sang")
    assert(age == 38)

    assert((tuples match { case (a, b, c) => a }) == "Sang")
  }

  test("Tuple signature") {
    def returnTuple: (String, Int) = ("A", 1)

    assert(returnTuple == ("A", 1))
  }

  test("Splitting Tuple Seq") {
    val (a, b) = Seq(("a", 1), ("b", 2)).unzip

    assert(a == Seq("a", "b"))
    assert(b == Seq(1, 2))

    val (_, _, c) = Seq(("a", 1, 0), ("b", 2, 0)).unzip3

    assert(c == Seq(0, 0))
  }

}
