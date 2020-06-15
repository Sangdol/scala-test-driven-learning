import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Range.Inclusive

class CollectionTest extends AnyFunSuite {

  test("List") {
    assert(List(1, 2).head == 1)
    assert(List(1, 2)(1) == 2)
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
  }

}
