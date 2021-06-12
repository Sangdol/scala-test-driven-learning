import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive
import scala.collection.mutable.ListBuffer

class ListAndSeqTest extends AnyFunSuite {

  test("Empty & Nil") {
    assert(List.empty == Nil)
    assert(List.empty eq Nil)
  }

  test("Seq") {
    // Scala's Seq is Java's List
    // Scala's List is Java's LinkedList
    // https://stackoverflow.com/questions/10866639/difference-between-a-seq-and-a-list-in-scala
    assert(Seq(1, 2).head == 1)
  }

  test("Range: to, until, and by") {
    val to = 1 to 5

    assert(to.isInstanceOf[Range])
    assert(to.isInstanceOf[Inclusive])
    assert(to.sum[Int] == 15)

    assert(to.head == 1)
    assert(to.tail == (2 to 5))

    val range = Range(0, 5)
    assert(range.sum == 10)
    assert(range == (0 until 5))

    val evens = 0 until 5 by 2
    assert(evens.sum == 6)
  }

  test("List immutable") {
    assert(List() == Nil)

    assert(List(1, 2)(1) == 2)

    assert(List(2).::(1) == List(1, 2))
    assert(List(1) :+ 2 == List(1, 2)) // append
    assert(1 :: List(2) == List(1, 2))
    assert(1 :: 2 :: List(3) == List(1, 2, 3))

    // Returns List
    assert(List(1, 2) ::: List(3) == List(1, 2, 3))
  }

  test("List iterable") {
    assert(List(1, 2).head == 1)

    assert(List(1, 2).forall(_ > 0))
    assert(!List(1, 2).forall(_ > 1))
    assert(List(1, 2).exists(_ > 1))
    assert(List(1, 2, 3).scanLeft(0)(_ + _) == List(0, 1, 3, 6))

    // Returns Iterable
    assert(List(1, 2).concat(List(3)) == List(1, 2, 3))
    assert(List(1, 2).++(List(3)) == List(1, 2, 3)) // alias for concat

    // 0
    // 3,0
    // 5,3,0
    // 6,5,3,0
    assert(List(1, 2, 3).scanRight(0)(_ + _) == List(6, 5, 3, 0))

    assert((0 to 2).sum == 3)

    val it = (0 to 2) grouped 2
    assert(it.next == List(0, 1))
    assert(it.next == List(2))
    assert(it.isEmpty)

    val it2 = (0 to 2) sliding 2
    assert(it2.next == List(0, 1))
    assert(it2.next == List(1, 2))
    assert(it2.isEmpty)
  }

  test("List Factory") {
    assert(List.fill(2)(3) == List(3, 3))
    assert(List.fill(2)({ 1 + 2 }) == List(3, 3))
  }

  test("fold, unfold") {
    assert(List(1, 2, 3).fold(100)(_ + _) == 106)

    // unfold[A, S](init: S)(f: S => Option[(A, S)]): CC[A]
    // A: value
    // S: conditional value
    // CC: container (List)
    assert(
      List.unfold(2)(n => if (n > 0) Some(n + 1, n - 1) else None) ==
        List(3, 2)
    )

    // This will run infinitely if it were List
    // since there's no condition to stop it.
    assert(
      LazyList.unfold(100)(n => Some((1, n + 10))).take(2) ==
        LazyList(1, 1)
    )
  }

  test("zip") {
    assert(List(1, 2).zip(List(2, 1)) == List((1, 2), (2, 1)))
  }

  test("find") {
    assert(List(1, 2).find(_ == 3).getOrElse(None) == None)
    assert(List(1, 2).find(_ == 1).getOrElse(None) == 1)
  }

  test("from") {
    assert(LazyList.from(0).take(3) == List(0, 1, 2))
  }

  test("Equality and identity") {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3)

    // identity
    assert(!(a eq b))

    // equality
    assert(a == b)

    val na: List[String] = Nil
    val nb: List[Int] = Nil

    // Nils are identical even of different types
    //noinspection ComparingUnrelatedTypes
    assert(na eq nb)
    assert(na == nb)
  }

  test("sort") {
    // This uses Java's sort method.
    assert(List(3, 1, 2).sorted == List(1, 2, 3))
  }

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
