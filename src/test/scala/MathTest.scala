import org.scalatest.funsuite.AnyFunSuite
import scala.math.Ordering.Implicits._

class MathTest extends AnyFunSuite {

  test("tuple comparison") {
    // https://stackoverflow.com/questions/11102393/how-to-lexicographically-compare-scala-tuples
    assert((1,2) < (1,3))
    assert((1,2) <= (1,2))
    assert((1,22) <= (2,2))
  }

}
