package fpinscala

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite

/**
 * ScalaCheck https://github.com/typelevel/scalacheck
 */

class ch8_1_scalacheck extends AnyFunSuite {
  test("ScalaCheck") {
    // How does this generate samples?
    // Can we get deterministic results?
    val intList = Gen.listOf(Gen.choose(0,100))
    val prop =
      forAll(intList){ns => ns.reverse.reverse == ns} &&
      forAll(intList){ns => ns.reverse.headOption == ns.lastOption}

    val failProp = forAll(intList){ns => ns.reverse == ns}

    // How could I use this with a test library?
    prop.check
    // Why does it say it falsified after n passed tests?
    failProp.check
  }

  test("8.1") {
    // Reversing a list and summing it should give the same result as
    // summing the original, nonreversed list.
    //  => sum(list.reverse) == sum(list)

    // What should the sum be if all elements of the list are the same value?
    //  => sum([11] * 10) == 11 * 10
    //  => How could I generate this input?

    // Can you think of other properties?
    //  => sum(range(n + 1)) == n(n+1)/2
  }

  test("8.2") {
    // What properties specify a function that finds the maximum of a List[Int]?
    //  => forAll(list){n => n <= max(list)}
  }


}
