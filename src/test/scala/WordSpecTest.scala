/**
 * ScalaTest At A Glance
 * http://www.scalatest.org/at_a_glance/WordSpec
 */
// '_' wildcard imports
import org.scalatest._

class WordSpecTest extends WordSpec {
  "empty" should {
    "have size 0" in {
      assert(Set.empty.isEmpty)
    }
  }
}
