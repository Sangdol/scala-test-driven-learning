import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap
import scala.language.existentials

class TypeTest extends AnyFunSuite {
  // https://stackoverflow.com/a/19388313/524588
  //noinspection ScalaUnusedSymbol
  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

  // manifest.runtimeClass.getSimpleName is better
  def shortManOf[T: Manifest](t: T): String =
    manOf(t).toString.split('.').last

  class TypeContainer[A](val a: A)(implicit
      manifest: scala.reflect.Manifest[A]
  ) {
    def contents: String = manifest.runtimeClass.getSimpleName
  }

  test("getClass") {
    assert(Nil.getClass.toString == "class scala.collection.immutable.Nil$")
    assert(Nil.getClass.getSimpleName == "Nil$")
    assert(Nil.getClass.getCanonicalName == "scala.collection.immutable.Nil$")

    assert(Int.getClass.toString == "class scala.Int$")
  }

  test("classOf") {
    assert(classOf[Int].toString == "int")
  }

  test("isInstanceOf function") {
    def addWithSyntaxSugar(x: Int) = (y: Int) => x + y

    // This is possible since parameter types are unknown at runtime.
    assert(addWithSyntaxSugar(1).isInstanceOf[Function1[_, _]])
    assert(addWithSyntaxSugar(1).isInstanceOf[(_) => _])
    assert(addWithSyntaxSugar(1).isInstanceOf[_ => _])
    assert(addWithSyntaxSugar(1).isInstanceOf[Int => Int])
  }

  test("isInstanceOf class") {
    // https://www.scala-exercises.org/std_lib/type_signatures
    trait Randomizer[A] {
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int] {
      def draw(): Int = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    assert(intRand.isInstanceOf[IntRandomizer])
    assert(intRand.isInstanceOf[Randomizer[Int]])
    assert(intRand.draw().isInstanceOf[Int])
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
    assert(new TypeContainer(u).contents == "G$1")

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

  test("type inference") {
    // Two ways
    val h1: HashMap[String, Int] = new HashMap
    val h2 = new HashMap[String, Int]

    assert(h1 == h2)

    // Scala can't infer all types in part due to subtype polymorphism (inheritance).
    //
    // When explicit type annotations are required
    // 1. No value (val book: String, var count: Int)
    // 2. Method parameters
    // 3. Method return type in the following cases
    //    a. explicit return
    //    b. recursive
    //    c. overloaded methods and one is calling another
    //    d. when the inferred return type is more general than intended e.g., Any
    //       (when you want to specify)

    object StringUtilV1 {
      def joiner(strings: String*) = strings.mkString("-")
      // overloaded - the return type is must
      def joiner(strings: List[String]): String = joiner(strings: _*)
    }

    assert(StringUtilV1.joiner(List("1", "2")) == "1-2")

  }

  test("Abstract Types") {
    // Parameterized types: for containers
    // Abstract Type: for type families
    trait exampleTrait {
      type t1
      type t2 >: t3 <: t1 // is `<: t1` needeD?
      type t3 <: t1
      type t4 <: Seq[t1]
      // type t5 = +AnyRef // Error: Can't use variance annotations

      val v1: t1
      val v2: t2
      val v3: t3
      val v4: t4
    }

    trait T1 { val name1: String }
    trait T2 extends T1 { val name2: String }
    case class C(name1: String, name2: String) extends T2

    object example extends exampleTrait {
      type t1 = T1
      type t2 = T2
      type t3 = C
      type t4 = Vector[T1]
      val v1 = new T1 { val name1 = "T1" }
      val v2 = new T2 { val name1 = "T1"; val name2 = "T2" }
      val v3 = C("T1", "T2")
      val v4 = Vector(v1)
    }

    test("Self-type annotations") {
      abstract class SubjectObserver {
        type S <: Subject
        type O <: Observer

        trait Subject {
          // Assuming that Subject will be an instance of the subtype S.
          // `_.receiveUpdate(self)` can't work without this.
          self: S =>

          private var observers = List[O]()

          def addObserver(observer: O) = observers ::= observer

          def notifyObservers() = observers.foreach(_.receiveUpdate(self))
        }

        trait Observer {
          def receiveUpdate(subject: S)
        }
      }
    }
  }
}
