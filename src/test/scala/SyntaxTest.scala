import org.scalatest.funsuite.AnyFunSuite

object CubeCalculator extends App {
  def cube(x: Int) = {
    x * x * x
  }
}

// https://docs.scala-lang.org/getting-started/intellij-track/testing-scala-in-intellij-with-scalatest.html
class SyntaxTest extends AnyFunSuite {
  test("CubeCalculator.cube") {
    assert(CubeCalculator.cube(3) === 27)
  }

  // https://docs.scala-lang.org/cheatsheets/
  test("Variables") {
    var variable = 5
    variable = 3

    val constant = 3

    assert(variable === constant)
  }

  test("Anonymous function") {
    assert(((x: Int) => x * x)(3) === 9)

    // underscore
    assert((1 to 3).map(_ * 2) === Array(2, 4, 6))  // positional match
    assert((1 to 3).reduceLeft(_ + _) === 6)  // positional match
    assert((1 to 3).map(x => x * x) === Array(1, 4, 9))  // no underscore possible in this case

    // block style
    assert((1 to 3).map { x =>
      val y = x * 2
      y
    } === Array(2, 4, 6))

    // pipeline style
    val r = (1 to 3) filter {
      _ % 2 == 0
    } map {
      _ * 2
    }

    assert(r === Array(4))

    // This is not possible for some reason.
//    assert((1 to 3) filter {
//      _ % 2 == 0
//    } map {
//      _ * 2
//    } === Array(4))

  }

  test("Function call") {
    assert("Abc".toLowerCase === "abc")
    assert("Abc".toLowerCase() === "abc")
  }

  test("Array") {
    assert((1 to 3) === Array(1, 2, 3))
  }
}
