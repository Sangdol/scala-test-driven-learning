import org.scalatest.funsuite.AnyFunSuite

import scala.language.existentials

class TypeTest extends AnyFunSuite {
  // https://stackoverflow.com/a/19388313/524588
  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

  def shortManOf[T: Manifest](t: T): String =
    manOf(t).toString.split('.').last

  test("getClass") {
    assert(Nil.getClass.toString == "class scala.collection.immutable.Nil$")
    assert(Nil.getClass.getSimpleName == "Nil$")
    assert(Nil.getClass.getCanonicalName == "scala.collection.immutable.Nil$")

    assert(Int.getClass.toString == "class scala.Int$")
  }

  test("classOf") {
    assert(classOf[Int].toString == "int")
  }

  test("isInstanceOf") {
    def addWithSyntaxSugar(x: Int) = (y: Int) => x + y

    // This is possible since parameter types are unknown at runtime.
    assert(addWithSyntaxSugar(1).isInstanceOf[Function1[_, _]])
    assert(addWithSyntaxSugar(1).isInstanceOf[(_) => _])
    assert(addWithSyntaxSugar(1).isInstanceOf[_ => _])
    assert(addWithSyntaxSugar(1).isInstanceOf[Int => Int])
  }

  test("Local Type Inference") {
    // https://dzone.com/articles/introduction-to-scala-type-system
    // This method has to have a type annotation for the return type
    // since the function call cannot infer the type
    // as Scala uses Local Type Inference unlike Haskell.
    def factorial(a: Int): Int = if (a <= 1) 1 else a * factorial(a - 1)

    assert(factorial(3) == 6)
  }

  test("sub-typing") {
    // It converts a lower type into a higher type.
    // Type Hierarchy Diagram https://dzone.com/articles/introduction-to-scala-type-system
    assert(shortManOf(List(10, 'a')) == "List[Int]")
    assert(shortManOf(List(10, 10.1)) == "List[Double]")
    assert(shortManOf(List(10, 10.1, "Hello")) == "List[Any]")
  }

  test("Existential Types 1") {
    // A: type variable
    sealed trait F[A]
    final case class SomeClass[A](a: A) extends F[A]

    val h = SomeClass("Hello"): F[String]
    assert(shortManOf(h) == "String]") // ???

    val i = SomeClass(1): F[Int]
    assert(shortManOf(i) == "TypeTest$F$1[Int]") // The first F of TypeTest.

    val b = SomeClass(true): F[Boolean]
    assert(shortManOf(b) == "TypeTest$F$1[Boolean]")
  }

  /**
    * https://dzone.com/articles/existential-types-in-scala
    *
    * Existential types are a way of abstracting over types.
    * They let you “acknowledge” that there is a type involved without
    * specifying exactly what it is, usually because you don’t know what it is
    * and you don’t need that knowledge in the current context.
    */
  test("Existential Types 2") {
    // Existential types are normal type variables, except that
    // the variable only shows up on the RHS of the declaration.
    type F = SomeClass[A] forSome { type A }

    sealed trait G

    final case class SomeClass[A](a: A) extends G

    case class User(name: String, age: Int, contact: String)

    val s = SomeClass("SomeString"): G
    assert(shortManOf(s) == "TypeTest$G$1")

    val u = SomeClass(User): G
    assert(shortManOf(u) == "TypeTest$G$1")

    // Type parameter in the method
    def getLengthWithType[T](x: Array[T]): Int = x.length

    // but I can call it without a type signature?
    assert(getLengthWithType(Array(1, 2, 3)) == 3)

    // No type parameter with existential type
    def getLength(x: Array[T] forSome { type T }): Int = x.length
    assert(getLength(Array(1, 2, 3)) == 3)

    // Martin Odersky himself:
    //  “Scala uses the erasure model of generics,
    //   just like Java, so we don’t see the type parameters anymore when programs are run.
    //   We have to do erasure because we need to interoperate with Java.
    //   But then what happens when we do reflection or want to express what goes on the in the VM?
    //   We need to be able to represent what the JVM does using the types we have in Scala,
    //   and existential types let us do that.”

    // https://www.drmaciver.com/2008/03/existential-types-in-scala/
    // “I want an Array, and I don’t care what type of things it contains”
    // This is exactly what existential types are for.
    // Short form
    def getLengthUnderline(x: Array[_]): Int = x.length
    assert(getLengthUnderline(Array(1, 2, 3)) == 3)
  }

}
