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
    //   Since it's at contravariant position.
    // Why is the type Fruit rather than Orange?
    //   Since its container type is Fruit when it was created.
    // Why I can assign a FruitBasket to an OrangeBasket?
    //   Since the value in it would be only accessible by its super type.
    val orangeBasket: TypeContainerMinus[Orange] =
      new TypeContainerMinus[Fruit](new Orange())
    assert(orangeBasket.contents == "Fruit$1")
  }

  test("Constructor variance") {
    class Covariance[+A](val a: A)
    val co = new Covariance(1)
    assert(co.a == 1)

    // 'a' is not accessible directly using 'val'
    // since it'd be covariance by being able to return the value.
    class Contravariance[-A, U >: A](a: A) {
      var u: U = a
    }
    val contra = new Contravariance[String, Any]("a")
    assert(contra.u.toString == "a")

    // You can also change the value since it's accessible
    // only by an upper type.
    contra.u = 1
    assert(contra.u.toString == "1")
  }

  test("Complex covariance") {
    // A is in contravariant position
    // since it'll be used as an argument.
//    case class Covariance[+A](a: A => Int)
//    case class Covariance[+A](a: Int => A => Int)

    case class Covariance[+A](a: Int => Int => A)
    case class Covariance2[+A](a: Int => Int => Int => A)
    case class Covariance3[+A, U <: A](a: Int => U => Int)
    case class Covariance4[+A, U <: A](a: Int => U => A)
  }

  test("Complex contravariance") {
    // A is in covariant position
    // since it'll be used as a return value.
//    case class Contravariance[-A](a: Int => A)

    case class Contravariance[-A](a: A => Int)
    case class Contravariance2[-A](a: Int => A => Int)
    case class Contravariance3[-A](a: Int => A => A => Int)
    case class Contravariance4[-A, U >: A](a: Int => A => A => U)
  }

}
