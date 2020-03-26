/**
 * http://doc.scalatest.org/3.1.0/org/scalatest/flatspec/AnyFlatSpec.html
 */
import org.scalatest.flatspec.AnyFlatSpec

class FlatSpecTest extends AnyFlatSpec {

  behavior of "An empty Set"

  it should "Any empty Set should have size 0" in {
    assert(Set.empty.isEmpty)
  }

  it should "product NoSucheElementException when head is invoked" in {
    assertThrows[NoSuchElementException] {
      Set.empty.head
    }
  }
}
