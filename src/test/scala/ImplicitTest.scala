import org.scalatest.funsuite.AnyFunSuite

class ImplicitTest extends  AnyFunSuite {

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
    implicit val rate = 0.5F

    def calcTax(amount: Float)(implicit rate: Float): Float = amount * rate

    assert(calcTax(10) == 5.0F)
  }

  test("Implicitly") {
    import math.Ordering

    case class MyList[A](list: List[A]) {
      def sortBy1[B](f: A => B)(implicit ord: Ordering[B]): List[A] =
        list.sortBy(f)(ord)

      // We need 'implicitly' since Ordering is not explicitly declared.
      def sortBy2[B : Ordering](f: A => B): List[A] =
        list.sortBy(f)(implicitly[Ordering[B]])
    }

    assert(MyList(List(1, 3, 2)).sortBy1(i => i) == List(1, 2, 3))
    assert(MyList(List(1, 3, 2)).sortBy2(i => i) == List(1, 2, 3))
  }

}
