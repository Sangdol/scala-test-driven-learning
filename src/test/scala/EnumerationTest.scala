import org.scalatest.funsuite.AnyFunSuite

class EnumerationTest extends AnyFunSuite {

  test("get name") {
    object Color extends Enumeration {
      type Color = Value
      val RED = Value("red")
    }

    assert(Color.RED.toString == "red")
  }

  test("case class") {

    object Multiple extends Enumeration {
      // Why private?
      final case class MultipleVal private (mid: String, name: String) extends super.Val(mid)

      type Multiple = MultipleVal

      val sang: Multiple.MultipleVal = MultipleVal("1", "sang")
      val hj: Multiple.MultipleVal = MultipleVal("2", "hj")
    }

    val ids = Multiple.values.map(_.toString)
    assert(ids == Set("1", "2"))

    val collectedIds = Multiple.values.collect {
      case Multiple.MultipleVal(id, _) => id
    }
    assert(collectedIds == Set("1", "2"))

    assert(Multiple.withName("1") == Multiple.sang)
    assert(Multiple.withName("1").asInstanceOf[Multiple.MultipleVal] == Multiple.sang)
  }
}
