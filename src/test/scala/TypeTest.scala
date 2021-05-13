import org.scalatest.funsuite.AnyFunSuite

class TypeTest extends AnyFunSuite {

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
  }

  test("sub-typing") {
    // https://stackoverflow.com/a/19388313/524588
    def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

    def shortManOf[T: Manifest](t: T): String =
      manOf(t).toString.split('.').last

    // It converts a lower type into a higher type.
    // Type Hierarchy Diagram https://dzone.com/articles/introduction-to-scala-type-system
    assert(shortManOf(List(10, 'a')) == "List[Int]")
    assert(shortManOf(List(10, 10.1)) == "List[Double]")
    assert(shortManOf(List(10, 10.1, "Hello")) == "List[Any]")
  }

}
