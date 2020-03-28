import org.scalatest.funsuite.AnyFunSuite

class NumberTest extends AnyFunSuite {

  test("max") {
    assert((1 max 2) == 2)
  }

  test("base") {
    // base36
    assert(BigInt(123).toString(36) == "3f")
  }

}
