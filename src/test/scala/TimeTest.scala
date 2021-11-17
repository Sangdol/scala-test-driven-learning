import org.scalatest.funsuite.AnyFunSuite

import java.time.{Duration, Instant}

class TimeTest extends AnyFunSuite {
  test("Instant toString") {
    val i = Instant.EPOCH

    assert(i.toString == "1970-01-01T00:00:00Z")
  }

  test("Instant calculation") {
    // https://stackoverflow.com/a/55780162/524588
    val instant1 = Instant.parse("2019-02-14T18:42:00Z");
    val instant2 = Instant.parse("2019-04-21T05:25:00Z");

    assert(Duration.between(instant1, instant2).toDays == 65)
  }
}
