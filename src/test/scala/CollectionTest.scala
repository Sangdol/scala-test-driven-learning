import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive

class CollectionTest extends AnyFunSuite {

  test("Range") {
    val range = 1 to 5

    assert(range.isInstanceOf[Range])
    assert(range.isInstanceOf[Inclusive])
    assert(range.sum[Int] == 15)

    assert(range.head == 1)
    assert(range.tail == (2 to 5))
  }

}
