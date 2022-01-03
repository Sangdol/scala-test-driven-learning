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

}
