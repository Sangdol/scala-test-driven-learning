package fpinscala

import fpinscala.Prop.{FailedCase, SuccessCount}
import org.scalatest.funsuite.AnyFunSuite

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount),SuccessCount]
}

case class Gen[A](smaple: State[RNG,A])

trait Gen[T] {
  def listOf[A](a: Gen[A]): Gen[List[A]]
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop
}

class ch8_2_testing extends AnyFunSuite {
  test("8.1") {
    // Reversing a list and summing it should give the same result as
    // summing the original, nonreversed list.
    //  => sum(list.reverse) == sum(list)

    // What should the sum be if all elements of the list are the same value?
    //  => sum([11] * 10) == 11 * 10
    //  => How could I generate this input?

    // Can you think of other properties?
    //  => sum(range(n + 1)) == n(n+1)/2

    // more
    //  sum(emptyList) == 0
    //  sum([a,b,c]) == sum([a]) + sum([b,c])
  }

  test("8.2") {
    // What properties specify a function that finds the maximum of a List[Int]?
    //  => n <= max(list)

    // more
    //  max([n]) == n
    //  max(list) is in list
    //  max(emptyList) => null or exception
  }

  test("8.3") {
    trait Prop {
      def check: Boolean

      // 8.3 - a new way to implement laziness
      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = Prop.this.check && p.check
      }
    }
  }

}
