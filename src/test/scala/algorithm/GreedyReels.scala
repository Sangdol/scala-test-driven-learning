package algorithm

import org.scalatest.funsuite.AnyFunSuite

class GreedyReels extends AnyFunSuite {

  def whichReels(p: Int): List[Int] = {
    case class Choice(p: Int, reels: List[Int])

    val lengths = List(10, 5, 2, 1)

    val finalChoice = lengths.foldLeft(Choice(p, List())) { (choice, len) =>
      val reels = List.fill(choice.p / len)(len)
      Choice(choice.p % len, choice.reels ::: reels)
    }

    finalChoice.reels
  }

  test("whichReels") {
    assert(whichReels(1) == List(1))
    assert(whichReels(2) == List(2))
    assert(whichReels(3) == List(2, 1))
    assert(whichReels(29) == List(10, 10, 5, 2, 2))
  }

  def whichReelsTuple(p: Int): List[Int] = {
    List(10, 5, 2, 1)
      .foldLeft((p, List[Int]())) { (acc, len) =>
        val (remaining, reels) = acc
        val newReels = List.fill(remaining / len)(len)
        (remaining % len, reels ::: newReels)
      }
      ._2
  }

  test("whichReels") {
    assert(whichReelsTuple(1) == List(1))
    assert(whichReelsTuple(2) == List(2))
    assert(whichReelsTuple(3) == List(2, 1))
    assert(whichReelsTuple(29) == List(10, 10, 5, 2, 2))
  }
}
