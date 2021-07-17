import org.scalatest.funsuite.AnyFunSuite

class SetTest extends AnyFunSuite {

  test("List to set") {
    // Wrong
    assert(Set(List(1, 1, 2)) == Set(List(1, 1, 2)))

    // Right
    assert(List(1, 1, 2).toSet == Set(1, 2))
  }

}
