package algorithm

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class Fibonacci extends AnyFunSuite {

  def fibRecur(i: Int): Int = {
    def fib(i: Int): Int = {
      if (i < 2) i
      else fib(i - 1) + fib(i - 2)
    }

    fib(i)
  }

  test("fibRecur") {
    assert(fibRecur(0) == 0)
    assert(fibRecur(1) == 1)
    assert(fibRecur(2) == 1)
    assert(fibRecur(3) == 2)
    assert(fibRecur(4) == 3)
  }

  def fibTailRecur(i: Int): Int = {
    @tailrec
    def fib(i: Int, curr: Int, next: Int): Int = {
      if (i < 1) curr
      else fib(i - 1, next, curr + next)
    }

    fib(i, 0, 1)
  }

  test("fibTailRecur") {
    assert(fibTailRecur(0) == 0)
    assert(fibTailRecur(1) == 1)
    assert(fibTailRecur(2) == 1)
    assert(fibTailRecur(3) == 2)
    assert(fibTailRecur(4) == 3)
  }

  def fibNonRecur(i: Int): Int = {
    if (i < 2) return i

    var prev = 0
    var curr = 1

    // 2 -> 1
    // 3 -> 2
    for (_ <- 1 until i) {
      val temp = prev
      prev = curr
      curr = temp + curr
    }

    curr
  }

  test("fibNonRecur") {
    assert(fibNonRecur(0) == 0)
    assert(fibNonRecur(1) == 1)
    assert(fibNonRecur(2) == 1)
    assert(fibNonRecur(3) == 2)
    assert(fibNonRecur(4) == 3)
  }

  test("fib stream") {
    // https://medium.com/swlh/applying-a-function-to-just-one-previous-term-in-a-scala-lazy-collection-59db607a05c2
    // lazy is needed for IntelliJ to avoid "forward reference error"
    // why?
    lazy val fib: LazyList[Int] =
      0 #:: 1 #:: fib.zip(fib.tail).map(t => t._1 + t._2)

    assert(fib.take(5).toList == LazyList(0, 1, 1, 2, 3))
  }
}
