import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

/**
  * https://www.scalatest.org/scaladoc/3.1.2/org/scalatest/funsuite/AnyFunSuite.html
  */
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

  test("Mocking trait") {
    // https://dzone.com/articles/scala-replacing-trait-fake-one

    trait T {
      val hello = "hello"
    }

    class C extends T {
      def helloWorld: String = s"$hello world"
    }

    trait FakeT extends T {
      override val hello = "hallo"
    }

    val c = new C with FakeT

    assert(c.helloWorld == "hallo world")
  }

  test("fail") {
    assertThrows[Throwable](fail())
  }

}
