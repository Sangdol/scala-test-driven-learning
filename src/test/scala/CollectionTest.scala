import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive

class CollectionTest extends AnyFunSuite {

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

  test("Range") {
    val range = 1 to 5

    assert(range.isInstanceOf[Range])
    assert(range.isInstanceOf[Inclusive])
    assert(range.sum[Int] == 15)

    assert(range.head == 1)
    assert(range.tail == (2 to 5))
  }

  test("Map") {
    // Immutable map
    val scores = Map("Sang" -> 100)

    assert(scores == Map(("Sang", 100)))
    assert(scores("Sang") == 100)
    assert(scores.contains("Sang"))
    assert(!scores.contains("HJ"))
    assert(scores.getOrElse("HJ", 110) == 110)

    val scoresWithDefault = scores.withDefaultValue(10)

    assert(scoresWithDefault("Miyu") == 10)

    val scoresWithDefaultFun = scores.withDefault(_.length)

    assert(scoresWithDefaultFun("Yang") == 4)
  }

  test("Mutable Map") {
    val scores = scala.collection.mutable.Map("Sang" -> 100)

    assert(scores == Map(("Sang", 100)))

    scores += (("HJ", 110), ("Yang", 90))

    assert(scores.size == 3)

    scores("Miyu") = 90

    assert(scores.size == 4)

    scores -= "Sang"

    assert(scores.size == 3)
  }

  test("Map iteration") {
    val scores = Map("Sang" -> 100, "HJ" -> 110)
    var names = ""
    var sum = 0

    for ((k, v) <- scores) {
      names += k
      sum += v
    }

    assert(names == "SangHJ")
    assert(sum == 210)

    names = ""
    sum = 0
    for (k <- scores.keySet) names += k
    for (v <- scores.values) sum += v

    assert(names == "SangHJ")
    assert(sum == 210)

    assert(scores.keySet.isInstanceOf[Set[String]])
    assert(scores.values.isInstanceOf[Iterable[Int]])
  }

  test("Tuples") {
    val tuples = ("Sang", 38, 0.1)

    assert(tuples._1 == "Sang")
    assert(tuples._2 == 38)

    val (name, age, _) = tuples

    assert(name == "Sang")
    assert(age == 38)

    assert((tuples match { case (a, b, c) => a }) == "Sang")
  }

}
