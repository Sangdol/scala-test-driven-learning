import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

/**
  * scala.collection.mutable
  * https://dotty.epfl.ch/api/scala/collection/mutable.html
  */
class MutableSeqListArrayTest extends AnyFunSuite {

  test("ListBuffer - mutable list") {
    val lb = ListBuffer[Int]()

    lb += 1

    assert(lb == List(1))

    lb ++= List(2, 3)

    assert(lb == List(1, 2, 3))

    lb --= List(1, 3)

    assert(lb == List(2))
  }

}
