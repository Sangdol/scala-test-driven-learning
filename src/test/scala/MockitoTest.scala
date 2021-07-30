import org.mockito.MockitoSugar
import org.scalatest.wordspec.AnyWordSpec

// AnyWordSpec
// https://www.scalatest.org/scaladoc/3.2.0/org/scalatest/wordspec/AnyWordSpec.html
class MockitoTest extends AnyWordSpec with MockitoSugar  {

  class Person {
    def hi(): String = "hi"
  }

  trait Setup {
    val person1 = mock[Person]
  }

  "Mockito" when {
    "verify" should {
      // Why is ResetMocksAfterEachTest needed?
      // https://github.com/mockito/mockito-scala#orgmockitointegrationsscalatestresetmocksaftereachtest--orgmockitointegrationsscalatestresetmocksaftereachasynctesto
      "work for multiple test 1" in new Setup {
        person1.hi()

        verify(person1).hi()
      }

      "work for multiple test 2" in new Setup {
        person1.hi()

        verify(person1).hi()
      }
    }
  }

}
