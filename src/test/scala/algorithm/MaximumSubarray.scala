package algorithm

import org.scalatest.funsuite.AnyFunSuite

class MaximumSubarray extends AnyFunSuite {

  def bruteForce(input: Array[Int]): Int = {
    // O(n^2)
    val allSums =
      for (
        i <- input.indices;
        j <- i + 1 to input.length
      )
        yield input.slice(i, j).sum

    allSums.max
  }

  def maxAcross(left: Vector[Int], right: Vector[Int]): Int = {
    val leftSums = for (i <- 1 to left.length) yield left.takeRight(i).sum
    val rightSums = for (i <- 1 to right.length) yield right.take(i).sum
    leftSums.max + rightSums.max
  }

  // nlog(n)
  def divideAndConquer(input: Vector[Int]): Int = {
    input match {
      case Vector(x) => x
      case _ =>
        val (left, right) = input.splitAt(input.length / 2)
        val maxLeft = divideAndConquer(left)
        val maxRight = divideAndConquer(right)
        val maxA = maxAcross(left, right)
        List(maxLeft, maxRight, maxA).max
    }
  }

  // can this be more functional?
  def kadane(input: Vector[Int]): Int = {
    var max_global = input(0)
    var max_current = input(0)

    input.foreach { x =>
      max_current = x.max(max_current + x)
      max_global = max_current.max(max_global)
    }

    max_global
  }

  test("brute force") {
    assert(bruteForce(Array(-1, 10, 8, 100, -20)) == 118)
    assert(bruteForce(Array(-1, 10, 8, -50, 100, -20)) == 100)
    assert(bruteForce(Array(-1, 10, 80, -50, 100, -20)) == 140)
  }

  test("divide and conquer") {
    assert(divideAndConquer(Vector(-1, 10, 8, 100, -20)) == 118)
    assert(divideAndConquer(Vector(-1, 10, 8, -50, 100, -20)) == 100)
    assert(divideAndConquer(Vector(-1, 10, 80, -50, 100, -20)) == 140)
  }

  test("kadane") {
    assert(kadane(Vector(-1, 10, 8, 100, -20)) == 118)
    assert(kadane(Vector(-1, 10, 8, -50, 100, -20)) == 100)
    assert(kadane(Vector(-1, 10, 80, -50, 100, -20)) == 140)
  }

}
