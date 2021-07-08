package lib

/**
 * ScalaTest At A Glance
 * http://www.scalatest.org/at_a_glance/WordSpec
 *
 * Latest
 * http://doc.scalatest.org/3.1.0/org/scalatest/wordspec/AnyWordSpec.html
 */
// '_' wildcard imports
import org.scalatest.wordspec.AnyWordSpec

class WordSpecTest extends AnyWordSpec {
  "A Set" when {
    "empty" should {
      "have size 0" in {
        assert(Set.empty.isEmpty)
      }

      "produce NoSucheElementException when head is invoked" in {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
