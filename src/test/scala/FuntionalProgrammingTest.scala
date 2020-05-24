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
      def go(n: Int, curr: Int, next: Int): Int =
        if (n == 0) curr
        else go(n-1, next, curr+next)

      go(n, 0, 1)
    }

    assert(fib2(0) == 0)
    assert(fib2(1) == 1)
    assert(fib2(2) == 1)
    assert(fib2(3) == 2)
    assert(fib2(4) == 3)
    assert(fib2(5) == 5)
  }

  test("isSorted") {
    // exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

      @annotation.tailrec
      def loop(n: Int): Boolean =
        if (n+1 >= as.length) true
        else if (!ordered(as(n), as(n+1))) false
        else loop(n+1)

      loop(0)
    }

    assert(isSorted(Array(1, 2, 3), (a:Int, b:Int) => a < b))
  }

  test("partial") {
    def partial1[A,B,C](a: A, f: (A, B) => C): (B) => C =
      b => f(a, b)

    def add5 = partial1(5, math.addExact)

    assert(add5(3) == 8)
  }

  test("currying") {
    // exercise 2.3
    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

    def addCurry = curry(math.addExact)
    def add5 = addCurry(5)

    assert(add5(3) == 8)
  }
}
