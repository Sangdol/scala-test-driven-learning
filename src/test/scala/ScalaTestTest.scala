import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class ScalaTestTest extends AnyFunSuite {

  test("assertResult") {
    val result = 3

    // to differentiate expected from actual values (compared to assert)
    assertResult(3)(result)
  }

  test("should") {
    // https://www.scala-exercises.org/std_lib/asserts
    val result = 3

    result should equal(3)
    result should ===(3)
    result should be(3)
    result shouldEqual 3
    result shouldBe 3
  }

}
