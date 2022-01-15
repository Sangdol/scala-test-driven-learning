import org.scalatest.funsuite.AnyFunSuite

class OptionTest extends AnyFunSuite {

  // https://alvinalexander.com/scala/how-use-fold-scala-option-some-none-syntax/
  test("fold and getOrElse") {
    def f (i: Int) = i + 1

    val two = Some(2)
    val DEFAULT = 100

    assert(two.fold(DEFAULT)(f) == 3)
    assert(two.fold({
      val a = 1
      val b = 2
      a + b
    })(f) == 3)
    assert(two.map(f).getOrElse(DEFAULT) == 3)
  }

  test("get") {
    assertThrows[Exception](None.get)
  }

  test("option empty") {
    val none = Option.empty

    assert(none.getClass.toString == "class scala.None$")

    // This is needed sometimes...
    // https://stackoverflow.com/a/24797748/524588
    val str = Option.empty[String]

    assert(str.getClass.toString == "class scala.None$")
  }

  test("orElse, getOrElse") {
    val two = Some(2)
    val someTwo = two.orElse(Some(3))
    val stillTwo = two.getOrElse(3)

    assert(someTwo == Some(2))
    assert(stillTwo == 2)
  }

  test("many options") {
    val o1 = Some(1)
    val o2 = Option.empty[Int]
    val o3 = Some(3)

    val partialSum = (o1, o2, o3) match {
      case (Some(one), Some(two), Some(three)) => one + two + three
      // how could I put 'None' here?
      // pattern type is incompatible with expected type;
      //  [error]  found   : None.type
      //  [error]  required: Some[Int]
      //case (Some(one), Some(two), _) => one + two
      case (Some(one), Some(two), _) => one + two
      case (Some(one), None, Some(three)) => one + three
      case _ => 0
    }

    assert(partialSum == 4)
  }
}
