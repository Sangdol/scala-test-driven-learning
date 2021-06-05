import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable

class ImplicitTest extends AnyFunSuite {

  /**
    * https://docs.scala-lang.org/tour/implicit-parameters.html
    */
  test("Implicit Parameters") {
    abstract class Monoid[A] {
      def add(x: A, y: A): A
      def unit: A
    }

    object ImplicitObject {
      def sum[A](list: List[A])(implicit m: Monoid[A]): A = {
        if (list.isEmpty) m.unit
        else m.add(list.head, sum(list.tail))
      }
    }

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      override def add(x: String, y: String): String = x + y

      override def unit: String = ""
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def add(x: Int, y: Int): Int = x + y

      override def unit: Int = 0
    }

    // Monoid have to be visible here.
    assert(ImplicitObject.sum(List("A", "B")) == "AB")
    assert(ImplicitObject.sum(List(1, 2, 3)) == 6)
  }

  test("Implicit variable") {
    implicit val rate: Float = 0.5f

    def calcTax(amount: Float)(implicit rate: Float): Float = amount * rate

    assert(calcTax(10) == 5.0f)
  }

  test("Implicitly") {
    import math.Ordering

    case class MyList[A](list: List[A]) {
      def sortBy1[B](f: A => B)(implicit ord: Ordering[B]): List[A] =
        list.sortBy(f)(ord)

      // We need 'implicitly' since Ordering is not explicitly declared.
      def sortBy2[B: Ordering](f: A => B): List[A] =
        list.sortBy(f)(implicitly[Ordering[B]])
    }

    assert(MyList(List(1, 3, 2)).sortBy1(i => i) == List(1, 2, 3))
    assert(MyList(List(1, 3, 2)).sortBy2(i => i) == List(1, 2, 3))
  }

  test("evidence") {
    // Copied from TraversableOnce (TraversableOnce is deprecated for some reason)
    // ev is synthesized by the compiler
    def toMap[A, T, U](lst: IterableOnce[A])(implicit
        ev: <:<[A, (T, U)] // A has to be a pair (subtype of (T, U)).
    ): Map[T, U] = {
      val b = immutable.Map.newBuilder[T, U]
      for (x <- lst)
        b += x

      b.result()
    }

    assert(toMap(List("a" -> 1)) == Map("a" -> 1))

    // Compiler error: no implicit arguments of type: String <:< (T, U)
//    assert(toMap(List("a"))
  }

}
