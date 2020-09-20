import org.scalatest.funsuite.AnyFunSuite


class VarianceTest extends AnyFunSuite {
  /**
   * https://docs.scala-lang.org/tour/lower-type-bounds.html
   */
  test("Lower type bounds") {

    sealed abstract class Animal {
      def hi(): String = "Hello"
    }

    case class Cat(name: String) extends Animal {
      def meow(): String = this.name + " meow"
    }

    case class Dog(name: String) extends Animal {
      def bark(): String = this.name + " bark"
    }

    trait Node[+B] {
      def prepend[U >: B](elem: U): Node[U]
    }

    case class ListNode[+B](h: B, t: Node[B]) extends Node[B] {
      // "elem: U" is a contravariant position and +B is a covariant sign.
      // So you cannot use "elem: B".
      // U is a supertype of B e.g., Animal and Dog.
      // U is invariant.
      // Since it returns animal the returned list also takes a dog and a cat.
      def prepend[U >: B](elem: U): ListNode[U] = ListNode(elem, this)

      // It can return ListNode[B] since it's in a covariant position.
      def prepend2[U >: B](elem: U): ListNode[B] = ListNode(h, this)

      def head: B = h

      def tail: Node[B] = t
    }

    case class Nil[+B]() extends Node[B] {
      def prepend[U >: B](elem: U): ListNode[U] = ListNode(elem, this)
    }

    val dogList = ListNode[Dog](Dog("dog"), Nil())
    val animalList: Node[Animal] = dogList
    animalList.prepend(Cat("cat"))

    assert(dogList.head.bark() == "dog bark")

    // It becomes an animal list.
    val catDogList = dogList.prepend(Cat("cat"))

    assert(catDogList.head.hi() == "Hello")
    // It cannot meow or bark anymore.
//    assert(catDogList.head.meow() == "cat meow")
//    assert(catDogList.head.bark() == "dog bark")
  }

}
