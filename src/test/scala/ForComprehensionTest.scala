import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class ForComprehensionTest extends AnyFunSuite {

  test("under the hood") {
    // foreach

    for {
      n <- List(1, 2, 3)
    } assert(n > 0)

    // map

    val ss = for {
      s <- List("a", "b")
    } yield s.toUpperCase()

    assert(ss == List("A", "B"))

    // flatMap

    val list = List("Aa", "Bb")
    val sc = for {
      s <- list
      c <- s
    } yield f"$c-${c.toUpper}"

    assert(sc == List("A-A", "a-A", "B-B", "b-B"))

    val sc2 = list.flatMap(_.toSeq map (c => f"$c-${c.toUpper}"))

    assert(sc2 == List("A-A", "a-A", "B-B", "b-B"))

    // flatMap with a guard (withFilter)

    val sc3 = for {
      s <- list
      c <- s
      if c.isLower
    } yield f"$c-${c.toUpper}"

    assert(sc3 == List("a-A", "b-B"))

    val sc4 = list.flatMap(_.toSeq withFilter (_.isLower) map (c => f"$c-${c.toUpper}"))

    assert(sc4 == List("a-A", "b-B"))

    // flatMap with steps

    val sc5 = for {
      s <- list
      c <- s
      if c.isLower
      c2 = f"$c-${c.toUpper}"
    } yield c2

    assert(sc5 == List("a-A", "b-B"))

    val sc6 = list.flatMap(_.toSeq withFilter (_.isLower) map (c => {
      val c2 = f"$c-${c.toUpper}"
      c2
    }))

    assert(sc6 == List("a-A", "b-B"))
  }

  test("translation rules") {
    // how @ works?
    val z @ (x, y) = 1 -> 2

    assert(z == (1 -> 2))
    assert(x == 1)
    assert(y == 2)

    // Translation steps
    // 1. pat <- expr.withFilter { case pat => true; case _ => false }
    // 2.1. expr1 map { case pat => expr2 } (with yield)
    // 2.2. expr1 foreach { case pat => expr2 }
    // 3. expr1 flatMap { case pat1 => for (pat2 <- expr2 ...) yield exprN }
    //    * more than one generator

    // From

    val m = Map("One" -> 1, "Two" -> 2)

    val list1 = for {
      (key, value) <- m
      i10 = value + 10
    } yield i10

    assert(list1 == List(11, 12))

    // Translation

    val list2 = for {
      (i, i10) <- for {
        x1 @ (key, value) <- m
      } yield {
        val x2 @ i10 = value + 10
        (x1, x2)
      }
    } yield i10

    assert(list2 == List(11, 12))
  }

}
