import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive
import scala.collection.mutable.ListBuffer

/**
  * Seq (trait)
  * - IndexedSeq: Vector, NumericRange, String, Range
  * - LinearSeq: List, Stream, Queue, Stack
  * https://stackoverflow.com/a/43457354/524588
  *
  * Performance Characteristics
  * https://docs.scala-lang.org/overviews/collections-2.13/performance-characteristics.html
  */
class SeqListTest extends AnyFunSuite {

  test("flatten") {
    // https://alvinalexander.com/scala/how-to-flatten-list-lists-in-scala-with-flatten-method/
    assert(Seq(Seq(1, 2), Seq(3, 4)).flatten == Seq(1, 2, 3, 4))

    assert(Seq(Map(1 -> 2), Map(3 -> 4)).flatten == Seq((1, 2), (3, 4)))
  }

  test("groupBy") {
    assert(
      Seq((1, "a"), (2, "b"), (3, "a")).groupBy(_._2) ==
        Map("a" -> Seq((1, "a"), (3, "a")), "b" -> Seq((2, "b")))
    )

    // A way to do distinctBy below Scala 2.13.
    assert(
      Seq((1, "a"), (2, "b"), (3, "a")).groupBy(_._2).flatMap(_._2.headOption).toList ==
        Seq((1, "a"), (2, "b"))
    )

    assert(
      Seq(Map(1 -> "a"), Map(2 -> "b"), Map(3 -> "a")).flatten.groupBy(_._2) ==
        Map("a" -> Seq((1, "a"), (3, "a")), "b" -> Seq((2, "b")))
    )
  }

  test("collect") {
    // http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-collect-function/
    val s = Seq("a", 1, "b")
    val ss = s.collect { case str: String => str }

    assert(ss == Seq("a", "b"))
  }

  test("flatMap") {
    val s = Seq(1, 2, 3)

    assert(s.flatMap { n => Seq(n, n + 1) } == Seq(1, 2, 2, 3, 3, 4))
  }

  test("Empty & Nil") {
    assert(List.empty == Nil)
    assert(List.empty eq Nil)
  }

  test("Seq") {
    // Scala's Seq is Java's List (interface)
    // Scala's List is Java's LinkedList (implementation)
    // https://stackoverflow.com/questions/10866639/difference-between-a-seq-and-a-list-in-scala
    assert(Seq(1, 2).head == 1)

    // Seq[Int] => List[Int]
    // List if optimized to prepend
    assert(Seq(1, 2) ++ Seq(3, 4) == Seq(1, 2, 3, 4)) // O(n)
    assert(Seq(1, 2) :+ 3 == Seq(1, 2, 3)) // O(n)
    assert(0 +: Seq(1, 2) == Seq(0, 1, 2)) // O(1)
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
    assert(List.fill(2) { 1 + 2 } == List(3, 3))
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

  test("span") {
    assert(List(7, 1).span(_ != 2) == (List(7, 1), List()))
    assert(List(7, 1).span(_ != 1) == (List(7), List(1)))
    assert(List(7, 1, 1).span(_ != 1) == (List(7), List(1, 1)))

    assert(
      List(7, 3, 1, 4, 1, 6).span(_ != 1) == (List(7, 3), List(1, 4, 1, 6))
    )
  }

  test("for-comprehension") {
    // https://docs.scala-lang.org/tour/for-comprehensions.html
    val evens =
      for (ns <- List(1, 2, 3) if ns % 2 == 0)
        yield ns

    assert(evens == List(2))

    val evensDouble =
      for {
        ns <- List(1, 2, 3) if ns % 2 == 0
        ds = ns * 2
      } yield ds

    assert(evensDouble == List(4))

    val evensTriple =
      for {
        ns <- List(1, 2, 3) if ns % 2 == 0
      } yield ns * 3

    assert(evensTriple == List(6))
  }

  test("groupMap") {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(
      l.groupMap(_ % 2 == 0)(identity) ==
        Map(true -> List(2, 4, 6, 8, 10), false -> List(1, 3, 5, 7, 9))
    )
  }

  test("groupMapReduce") {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(
      // def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): Map[K, B]
      // 1. group by the first argument result
      // 2. map the second argument result
      // 3. reduce the mapped results
      l.groupMapReduce(_ % 2 == 0)(identity)(_ + _) == Map(
        (false, 25),
        (true, 30)
      )
    )
  }

  test("groupMapReduce: list to key value map") {
    val l = List(1, 1, 2)

    // Diffiult way
    assert(
      l.groupMapReduce(_ * 2)(identity)((a, _) => a) ==
        Map(2 -> 1, 4 -> 2)
    )

    // Better way
    val keyValue = l.map(k => (k * 2, k)).toMap
    assert(keyValue == Map(2 -> 1, 4 -> 2))
  }

}
