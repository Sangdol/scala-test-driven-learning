package akkahttp

import akka.http.scaladsl.server.Directives.{
  complete,
  get,
  pathEndOrSingleSlash,
  pathPrefix
}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport

// Where is the document for FailFastCirceSupport?
object Routes extends FailFastCirceSupport {

  lazy val health = get {
    pathPrefix("health") {
      pathEndOrSingleSlash {
        // FailFastCirceSupport allows "complete" to take a string.
        complete("UP")
      }
    }
  }
}
