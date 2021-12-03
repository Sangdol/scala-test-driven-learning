package fpinscala

import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// Deprecated
// https://www.scalatest.org/user_guide/property_based_testing
// import org.scalatest.prop.PropertyChecks

/**
 * https://www.scalatest.org/user_guide/property_based_testing
 */
class PropertyBasedTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Table-driven property checks") {
    // https://www.scalatest.org/user_guide/table_driven_property_checks
    val adds = Table(
      ("a", "b", "c"), // First tuple: Column names
      (1, 2, 3),
      (-1, 2, 1)
    )

    forAll (adds) { (a: Int, b: Int, c: Int) =>
      assert(a + b == c)
    }
  }

}
