/**
 * Functional Programming in Scala
 */

import org.scalatest.funsuite.AnyFunSuite

class FuntionalProgrammingTest extends AnyFunSuite {

  test("@tailrec") {
    def factorial(n: Int): Int = {

      @annotation.tailrec
      def go(n: Int, acc: Int): Int = {
        if (n <= 0) acc
        else go(n-1, acc*n)
      }

      go(n, 1)
    }

    assert(factorial(3) == 6)
  }

  test("fibonacci") {
    // exercise 2.1
    def fib(n: Int): Int = {

      @annotation.tailrec
      def go(n: Int, i: Int, curr: Int, next: Int): Int = {
        if (n <= 1) n
        else if (n <= i) curr
        else go(n, i+1, next, curr+next)
      }

      go(n, 0, 0, 1)
    }

    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)

    def fib2(n: Int): Int = {

      @annotation.tailrec
      def go(n: Int, curr: Int, next: Int): Int = {
        if (n == 0) curr
        else if (n == 1) next
        else go(n-1, next, curr+next)
      }

      go(n, 0, 1)
    }

    assert(fib2(0) == 0)
    assert(fib2(1) == 1)
    assert(fib2(2) == 1)
    assert(fib2(3) == 2)
    assert(fib2(4) == 3)
    assert(fib2(5) == 5)
  }

}
