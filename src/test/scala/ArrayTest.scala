/**
 * https://docs.scala-lang.org/overviews/collections/arrays.html
 */

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq

class ArrayTest extends AnyFunSuite {

  test("Basic Operation") {
    // Array[Int] == int[] in Java
    val a = Array(1, 2, 3)

    assert(a(0) == 1)
    assert(a.length == 3)

    val b = a map (_ * 3)

    assert(b sameElements Array(3, 6, 9))

    val c = b filter (_ % 2 == 1)

    assert(c sameElements Array(3, 9))
    assert(c.reverse sameElements Array(9, 3))
  }

  test("Wrap and unwrap") {
    val seq: Seq[Int] = Array(1, 2, 3)

    // This is different from the doc https://docs.scala-lang.org/overviews/collections/arrays.html
    // probably due to the difference of version (2.13.0 and below)
    assert(seq == ArraySeq(1, 2, 3))
    assert(seq.toArray sameElements Array(1, 2, 3))
  }
}
