import org.scalatest.funsuite.AnyFunSuite

import java.time.Instant

class TimeTest extends AnyFunSuite {
  test("Instant toString") {
    val i = Instant.EPOCH

    assert(i.toString == "1970-01-01T00:00:00Z")
  }
}
