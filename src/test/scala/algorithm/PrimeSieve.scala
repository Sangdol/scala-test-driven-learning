package algorithm

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class PrimeSieve extends AnyFunSuite {

  def primeSieve(upTo: Int): List[Int] = {
    val primes = ListBuffer[Int]()
    val candidates = Array.fill(upTo + 1)(true)

    //
    // i = 2
    // primes = [2]
    // j = 4, 6, 8, 10 ...
    //
    // i = 3
    // primes = [2, 3]
    // j = 6, 9, 12 ...
    for (i <- 2 to upTo) {
      if (candidates(i)) {
        primes += i
        for (j <- (i * 2) to upTo by i) {
          candidates(j) = false
        }
      }
    }
    primes.toList
  }

  def primeSieveSeg(upTo: Int): List[Int] = {
    val primes = ListBuffer[Int]()
    val delta = math.sqrt(upTo).toInt
    val primesUpToSqrt = primeSieve(delta)

    primes ++= primesUpToSqrt

    // Why until?
    //   because of the way the candidates are generated in the loop
    for (i <- delta until upTo by delta) {
      // This is slower than filtering by indexes.
      //   Time Complexity: O(n x primesUpToSqrt)
      //     -> we might do this in O(n)
      val candidates = (i + 1) to (i + delta)
      primes ++= candidates.filter(c =>
        !primesUpToSqrt.exists(p => c % p == 0 || c > upTo)
      )
    }

    primes.toList
  }

  // recursive solution
  def primeSieveFunctional(n: Int): List[Int] = {
    // upTo = 100
    // c = 2, primesToSqrt = []
    // c = 3, primesToSqrt = []
    // c = 4, primesToSqrt = [2]
    // c = 5, primesToSqrt = [2]
    // ...
    // c = 10, primesToSqrt = [2, 3]
    lazy val primes: LazyList[Int] = 2 #:: LazyList
      .from(3)
      .filter { c =>
        val primesToSqrt = primes.takeWhile(p => p <= math.sqrt(c))
        !primesToSqrt.exists(p => c % p == 0)
      }

    primes.take(n).toList
  }

  test("primeSieve") {
    assert(primeSieve(5) == List(2, 3, 5))
    assert(primeSieve(7) == List(2, 3, 5, 7))
    assert(primeSieve(10) == List(2, 3, 5, 7))
    assert(primeSieve(15) == List(2, 3, 5, 7, 11, 13))
  }

  test("primeSieveSeg") {
    assert(primeSieveSeg(5) == List(2, 3, 5))
    assert(primeSieveSeg(7) == List(2, 3, 5, 7))
    assert(primeSieveSeg(10) == List(2, 3, 5, 7))
    assert(primeSieveSeg(15) == List(2, 3, 5, 7, 11, 13))
  }

  test("primeSieveFunctional") {
    assert(primeSieveFunctional(3) == List(2, 3, 5))
    assert(primeSieveFunctional(4) == List(2, 3, 5, 7))
    assert(primeSieveFunctional(4) == List(2, 3, 5, 7))
    assert(primeSieveFunctional(6) == List(2, 3, 5, 7, 11, 13))
  }
}
