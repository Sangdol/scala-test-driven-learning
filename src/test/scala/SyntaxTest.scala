import org.scalatest.funsuite.AnyFunSuite
import scala.math._
import scala.util.control.Breaks._

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

    val xmax, ymax = 100

    assert(xmax === 100)
    assert(ymax === 100)

    var a, b: String = "abc"

    assert(a.isInstanceOf[String])
    assert(b === "abc")
  }

  test("Anonymous function") {
    assert(((x: Int) => x * x)(3) === 9)

    // underscore
    assert((1 to 3).map(_ * 2) === Array(2, 4, 6))  // positional match
    assert((1 to 3).reduceLeft(_ / _) === 0)  // positional match
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
    // The rule of thumb is that a parameterless method
    // that doesn’t modify the object has no parentheses.
    assert("Abc".toLowerCase == "abc")
    assert("Abc".toLowerCase() == "abc")

    // Operator overloading is possible in Scala not like Java.
    assert(1 + 2 == 1.+(2))

    val x: BigInt = 123
    assert(x * x == 15129)

    // Function as a parameter
    // _ becomes Char.
    assert("Abc".count(_.isUpper) == 1)
  }

  test("Array") {
    assert((1 to 3) === Array(1, 2, 3))
  }

  test("singleton object - static in Java") {
    assert(pow(2, 2) == 4)

    // A package can have a package object.
    // See scala.math.package.scala
    assert(math.pow(2, 2) == 4)

    // You can omit 'scala' if the package is from 'scala'.
    assert(scala.math.pow(2, 2) == 4)

    // There's a companion object whose methods act just like static in Java.
    assert(BigInt.int2bigInt(1) == 1)
  }

  test("apply method") {
    val s = "hello"

    // See scala.collection.StringOps
    assert(s(0) == s.apply(0))
    assert(s(0) == 'h')

    assert(BigInt("123") == 123)
    assert(BigInt.apply("123") == 123)
  }

  test("if") {
    val n = 10
    val t = if (n > 0) "abc" else 10

    assert(t.isInstanceOf[String])

    // Every expression has a type.
    assert((if (1 > 0) 1 else 0).isInstanceOf[Int])
    assert((if (1 > 0) "a" else 0).isInstanceOf[Any])

    // Both are the same
    // Unit is equivalent to void in Java.
    assert((if (1 < 0) "a").isInstanceOf[Unit])
    assert((if (1 < 0) "a" else ()).isInstanceOf[Unit])
  }

  test("block") {
    // Blocks return the last expression.
    val block = {
      val a = 2
      var b = 3
      a * b
    }

    assert(block == 6)

    // An assignment returns a Unit value.
    // Unit is a type and the only value is '()'.
    assert({val a = 1} == ())
  }

  test("Loops") {
    var n = 10
    var sum = 0

    while (n > 0) {
      sum += n
      n -= 1
    }

    assert(sum == 55)

    sum = 0
    for (i <- 1 to 10) {
      sum += i
    }

    assert(sum == 55)

    var str = ""
    for (ch <- "Hi") {
      str += ch
    }

    assert(str == "Hi")

    // break is not normal in Scala
    // https://stackoverflow.com/questions/2742719/how-do-i-break-out-of-a-loop-in-scala
    sum = 0
    breakable {
      for (i <- 1 to 10) {
        sum += i
        if (i > 5) break
      }
    }

    assert(sum == 21)
  }
}
