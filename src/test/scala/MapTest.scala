import org.scalatest.funsuite.AnyFunSuite

class MapTest extends AnyFunSuite {

  test("flatMap") {
    val graph: Map[String, Seq[String]] =
      Map("a" -> Seq("b", "c"), "b" -> Seq("c"))

    // Why "(v, neighbours) =>" or "((v, neighbours)) =>" doesn't work?
    // Probably because the values in a tuple cannot spread into "((v, neighbours))".
    val edges: Map[String, String] = graph.flatMap {
      case (v, neighbours) => neighbours.map(n => (v, n))
    }

    // A longer form of above
    val edges2: Map[String, String] = graph.flatMap { element =>
      element match {
        case (v, neighbours) => neighbours.map(n => (v, n))
      }
    }

    // this is not a correct way to get edges?
    assert(edges.toSeq == Seq(("a", "c"), ("b", "c")))
    assert(edges == edges2)

    val edges3: List[(String, String)] = graph.toList.flatMap {
      case (v, neighbours) => neighbours.map(n => (v, n))
    }

    // This seems to be right.
    assert(edges3 == Seq(("a", "b"), ("a", "c"), ("b", "c")))
  }

  test("Map") {
    // Immutable map
    val scores = Map("Sang" -> 100)

    assert(scores == Map(("Sang", 100)))
    assert(scores("Sang") == 100)
    assert(scores.contains("Sang"))
    assert(!scores.contains("HJ"))
    assert(scores.getOrElse("HJ", 110) == 110)
  }

  test("concat") {
    val scores = Map[String, Int]()
    val scores1 = scores + ("Sang" -> 100)
    assert(scores1 == Map("Sang" -> 100))
  }

  test("default value") {
    val scores = Map[String, Int]()
    val scoresWithDefault = scores.withDefaultValue(10)

    assert(scoresWithDefault("Miyu") == 10)

    val scoresWithDefaultFun = scores.withDefault(_.length)

    assert(scoresWithDefaultFun("Yang") == 4)
  }

  test("Mutable Map") {
    var scores = scala.collection.mutable.Map("Sang" -> 100)

    assert(scores == Map(("Sang", 100)))

    scores = scores + (("HJ", 110), ("Yang", 90))

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

  test("Merge maps: override") {
    val m1 = Map("a" -> 1, "b" -> 2)
    val m2 = Map("a" -> 2, "c" -> 3)
    val merged = m1 ++ m2

    assert(merged == Map("a" -> 2, "b" -> 2, "c" -> 3))
  }

  test("Merge maps: groupMap") {
    // from Scala 2.13
    val m1 = Map("a" -> 1, "b" -> 2)
    val m2 = Map("a" -> 2, "c" -> 3)
    val merged = (m1.toSeq ++ m2).groupMap(_._1)(_._2)

    assert(merged == Map("a" -> Seq(1, 2), "b" -> Seq(2), "c" -> Seq(3)))
  }

  test("Add tuple") {
    val m = Map("a" -> 1)
    val t = ("b", 2)

    assert(m + t == Map("a" -> 1, "b" -> 2))

    // Need double parentheses
    // https://stackoverflow.com/questions/40221310/add-tuple-to-map/40222241
    assert(m + (("b", 2)) == Map("a" -> 1, "b" -> 2))
  }
}
