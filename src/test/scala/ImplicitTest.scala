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

}
