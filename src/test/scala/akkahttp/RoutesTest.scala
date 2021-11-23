package akkahttp

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RoutesTest extends AnyWordSpec with Matchers with ScalaFutures with ScalatestRouteTest {
  "Routes" should {
    "return OK" in {
      Get("/health") ~> Routes.health ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }
}
