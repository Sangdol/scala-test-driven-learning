import org.scalatest.funsuite.AnyFunSuite

class VarianceTest extends AnyFunSuite {

  class TypeContainer[A](val a: A)(implicit
      manifest: scala.reflect.Manifest[A]
  ) {
    def contents: String = manifest.runtimeClass.getSimpleName
  }

  class TypeContainerPlus[+A](val a: A)(implicit
      manifest: scala.reflect.Manifest[A]
  ) {
    def contents: String = manifest.runtimeClass.getSimpleName
  }

  // Can't receive a 'val' because it would be in a covariant position.
  // (Adding 'val' means it's accessible and returnable.)
  class TypeContainerMinus[-A](a: A)(implicit
      manifest: scala.reflect.Manifest[A]
  ) {
    def contents: String = manifest.runtimeClass.getSimpleName
  }

  // Now it's possible to use 'val' since U is an upper type.
  class TypeContainerMinus2[-A, U >: A](val a: U)(implicit
      manifest: scala.reflect.Manifest[A]
  ) {
    def contents: String = manifest.runtimeClass.getSimpleName
  }

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
      //
      // What if I want to make a dog list?
      //
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

  test("immutability") {
    case class Value[+T](v: T)

    val v = Value[Int](1)

    // Why mutable value doesn't work? ("Covariant type T occurs in contravariant position")
    //   Because Value[Animal] can have a dog or a cat.
//    case class Value[+T](var v: T)

    // Then why this doesn't work? "Contravariant type T occurs in covariant position in type T of value v"
//    case class Value[-T](var v: T)
  }

  test("Type Variance") {
    class Fruit

    class Orange extends Fruit

    val fruitBasket = new TypeContainer(new Orange())
    assert(fruitBasket.contents == "Orange$1")

    val fruitBasket2 = new TypeContainer[Fruit](new Orange())
    assert(fruitBasket2.contents == "Fruit$1")

    val fruitBasket3: TypeContainerPlus[Fruit] =
      new TypeContainerPlus[Orange](new Orange())
    assert(fruitBasket3.contents == "Orange$1")

    // Why can it pass an orange?
    // Why is the type Fruit rather than Orange?
    val fruitBasket4: TypeContainerMinus[Orange] =
      new TypeContainerMinus[Fruit](new Orange())
    assert(fruitBasket4.contents == "Fruit$1")
  }

}
