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
      // Context Bound https://docs.scala-lang.org/tutorials/FAQ/index.html
      def sortBy2[B: Ordering](f: A => B): List[A] =
        list.sortBy(f)(implicitly[Ordering[B]])
    }

    assert(MyList(List(1, 3, 2)).sortBy1(i => i) == List(1, 2, 3))
    assert(MyList(List(1, 3, 2)).sortBy2(i => i) == List(1, 2, 3))
  }

  test("evidence") {
    // Copied from TraversableOnce (TraversableOnce is deprecated for some reason)
    // ev is synthesized by the compiler
    // This is a way to constrain allowed types without requiring to conform to a supertype.
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

  test("Working around erasure") {
    object MM {
      implicit object IntMarker
      implicit object StringMarker

      // This is not possible without Markers due to type erasure.
      def m(seq: Seq[Int])(implicit i: IntMarker.type): Seq[Int] = seq
      def m(seq: Seq[String])(implicit s: StringMarker.type): Seq[String] = seq
    }

    assert(MM.m(List(1)) == List(1))
    assert(MM.m(List("a")) == List("a"))
  }

  test("Phantom type") {
    // https://medium.com/@maximilianofelice/builder-pattern-in-scala-with-phantom-types-3e29a167e863
    /**
      * A phantom type is a manifestation of abstract type that has no effect on the runtime.
      * These are useful to prove static properties of the code using type evidences.
      * As they have no effect on the runtime they can be erased from the resulting code
      * by the compiler once it has shown the constraints hold.
      */
    sealed trait DoorState
    sealed trait Open extends DoorState
    sealed trait Closed extends DoorState

    case class Door[State <: DoorState]() {
      def open(implicit ev: State =:= Closed) = Door[Open]()
      def close(implicit ev: State =:= Open) = Door[Closed]()
    }

    // No implicit arguments of type: Open =:= Closed
//    Door[Open].open

    assert(Door[Open].close == Door[Closed]())
    assert(Door[Closed].open == Door[Open]())
  }

  test("implicit arguments rules") {

    /**
      * 1. Only the last argument list, including the only list for a single-list method, can have implicit arguments.
      * 2. The implicit keyword must appear first and only once in the argument list.
      *    The list can’t have “nonimplicit” arguments followed by implicit arguments.
      * 3. All the arguments are implicit when the list starts with the implicit keyword.
      */
  }

  test("Type Class Pattern") {
    case class Name(firstName: String, lastName: String)
    case class Person(name: Name, age: Int)

    trait ToCSV {
      def toCSV: String
    }

    implicit class NameToCSV(name: Name) {
      def toCSV: String = f"${name.firstName},${name.lastName}"
    }

    implicit class PersonToCSV(person: Person) {
      def toCSV: String = {
        val name = person.name
        f"${name.firstName},${name.lastName},${person.age}"
      }
    }

    val name = Name("Sang", "Lee")
    val sang = Person(name, 38)

    assert(name.toCSV == "Sang,Lee")
    assert(sang.toCSV == "Sang,Lee,38")
  }

}
