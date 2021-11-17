import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
import org.scalatest.wordspec.AnyWordSpec

// AnyWordSpec
// https://www.scalatest.org/scaladoc/3.2.0/org/scalatest/wordspec/AnyWordSpec.html
class MockitoTest extends AnyWordSpec with MockitoSugar  {

  class Person {
    def hi(): String = "hi"
    def hello(who: String): String = s"Hello, $who!"
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

    // https://stackoverflow.com/a/14970545/524588
    "thenCallRealMethod()" should {
      "return real value" in new Setup {
        when(person1.hello(any[String])).thenCallRealMethod()

        assert(person1.hello("World") == "Hello, World!")
      }
    }
  }

}
