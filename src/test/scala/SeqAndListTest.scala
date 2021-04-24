import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive

class SeqAndListTest extends AnyFunSuite {

  test("Empty & Nil") {
    assert(List.empty == Nil)
    assert(List.empty eq Nil)
  }

  test("Seq") {
    // Scala's Seq is Java's List
    // Scala's List is Java's LinkedList
    // https://stackoverflow.com/questions/10866639/difference-between-a-seq-and-a-list-in-scala
    assert(Seq(1,2).head == 1)
  }

  test("Range") {
    val range = 1 to 5

    assert(range.isInstanceOf[Range])
    assert(range.isInstanceOf[Inclusive])
    assert(range.sum[Int] == 15)

    assert(range.head == 1)
    assert(range.tail == (2 to 5))
  }

  test("List") {
    assert(List() == Nil)
    assert(List(1, 2).head == 1)
    assert(List(1, 2)(1) == 2)

    assert(List(2).::(1) == List(1,2))
    assert(List(1) :+ 2 == List(1,2)) // append
    assert(1 :: List(2) == List(1,2))
    assert(1 :: 2 :: List(3) == List(1,2,3))

    assert(List(1,2).forall(_ > 0))
    assert(!List(1,2).forall(_ > 1))
    assert(List(1,2).exists(_ > 1))
    assert(List(1,2,3).scanLeft(0)(_ + _) == List(0,1,3,6))

    assert(List(1,2).concat(List(3)) == List(1,2,3))
    assert(List(1,2).++(List(3)) == List(1,2,3))
    assert(List(1,2) ::: List(3) == List(1,2,3))

    // 0
    // 3,0
    // 5,3,0
    // 6,5,3,0
    assert(List(1,2,3).scanRight(0)(_ + _) == List(6,5,3,0))
  }

}
