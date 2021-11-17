import org.scalatest.funsuite.AnyFunSuite

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}

class TimeTest extends AnyFunSuite {
  test("Instant toString") {
    val i = Instant.EPOCH

    assert(i.toString == "1970-01-01T00:00:00Z")
  }

  test("Instant calculation") {
    // https://stackoverflow.com/a/55780162/524588
    val instant1 = Instant.parse("2019-02-14T18:42:00Z")
    val instant2 = Instant.parse("2019-04-21T05:25:00Z")

    assert(Duration.between(instant1, instant2).toDays == 65)

    val instant3 = Instant.EPOCH
    val instant4 = Instant.EPOCH.plus(1, ChronoUnit.DAYS)

    assert(Duration.between(instant3, instant4).toDays == 1)

    val instant5 = Instant.parse("2021-08-14T12:35:49.713Z")
    assert(Duration.between(Instant.EPOCH, instant5).toDays == 18853)
  }
}
