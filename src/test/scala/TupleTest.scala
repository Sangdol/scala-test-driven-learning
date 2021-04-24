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

}
