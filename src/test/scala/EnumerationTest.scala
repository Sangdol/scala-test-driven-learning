import org.scalatest.funsuite.AnyFunSuite

class EnumerationTest extends AnyFunSuite {

  test("get name") {
    object Color extends Enumeration {
      type Color = Value
      val RED = Value("red")
    }

    assert(Color.RED.toString == "red")
  }

}
