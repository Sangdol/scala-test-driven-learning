import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Future}
import scala.math._
import scala.util.control.Breaks._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

// https://docs.scala-lang.org/getting-started/intellij-track/testing-scala-in-intellij-with-scalatest.html
class SyntaxTest extends AnyFunSuite {

  // https://docs.scala-lang.org/cheatsheets/
  test("Variables") {
    var variable = 5
    variable = 3

    val constant = 3

    assert(variable === constant)

    val xmax, ymax = 100

    assert(xmax === 100)
    assert(ymax === 100)

    val a, b: String = "abc"

    assert(a.isInstanceOf[String])
    assert(b === "abc")
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
      val b = 3
      a * b
    }

    assert(block == 6)

    // An assignment returns a Unit value.
    // Unit is a type and the only value is '()'.
    var a = 0
    assert({ a = 1 } == ())
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

    // Permutation
    // It's like a nested loop in one-line.
    var list = List[String]()
    for (i <- 1 to 2; j <- 1 to 2) {
      list = list.appended(f"$i$j")
    }
    assert(list === List("11", "12", "21", "22"))

    list = List[String]()
    for (i <- 1 to 2; j <- 1 to 2 if i != j) {
      list = list.appended(f"$i$j")
    }
    assert(list === List("12", "21"))

    list = List[String]()
    for (i <- 1 to 2; from = 3 - i; j <- from to 2) {
      list = list.appended(f"$i$j")
    }
    assert(list === List("12", "21", "22"))

    // Generators / for comprehension
    val vector = for (c <- "Hello"; i <- 0 to 1) yield (c + i).toChar

    assert(vector.isInstanceOf[Vector[Char]])
    assert(vector.size == 10)
  }

  // https://docs.scala-lang.org/tour/pattern-matching.html
  test("Case") {
    def matchTest(n: Int): String =
      n match {
        case 1 => "one"
        case 2 => "two"
        case _ => "other"
      }

    assert(matchTest(1) == "one")
    assert(matchTest(2) == "two")
    assert(matchTest(3) == "other")

    // Pattern guard: if ...
    def matchTestWithPatternGuard(n: Int, flag: Boolean): String =
      n match {
        case 1 if flag => "one"
        case 2 if flag => "two"
        case _         => "other"
      }

    assert(matchTestWithPatternGuard(1, flag = true) == "one")
    assert(matchTestWithPatternGuard(1, flag = false) == "other")

    def matchTestWithList(l: List[Int]): List[Any] =
      l match {
        case Nil        => Nil
        case first :: _ => List(first)
        case first :: second :: tail =>
          List(first, second, tail) // this will never be reached.
      }

    assert(matchTestWithList(Nil) == Nil)
    assert(matchTestWithList(List(1)) == List(1))
    assert(matchTestWithList(List(1, 2)) == List(1))
    assert(matchTestWithList(List(1, 2, 3)) == List(1))
  }

  test("for comprehensions with lists") {
    // https://docs.scala-lang.org/tour/for-comprehensions.html
    val evens =
      for (ns <- List(1, 2, 3) if ns % 2 == 0)
        yield ns

    assert(evens == List(2))
  }

  /**
    * https://stackoverflow.com/questions/19045936/scalas-for-comprehension-with-futures
    */
  test("for comprehension with futures") {

    /**
      * This runs sequentially.
      */
    val fsum1 = for {
      r1 <- Future(1)
      r2 <- Future(2)
    } yield r1 + r2

    assert(Await.result(fsum1, 1.second) == 3)

    /**
      * Parallel solution
      */
    val f1 = Future(1)
    val f2 = Future(2)
    val f3 = Future(3)

    val fsum2 = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield r1 + r2 + r3

    val sum = Await.result(fsum2, 1.second)

    // why this doesn't work?
    // https://stackoverflow.com/questions/15104536/how-does-20-seconds-work-in-scala
//    val sum = Await.result(fsum, 1 second)

    assert(sum == 6)
  }

  test("infix, prefix, and postfix") {
    // import scala.language.postfixOps is needed for the first expression
    // Precedence: Prefix >  Infix > Postfix
    val i = 1
    assert((-i to 2).toList == List(-1, 0, 1, 2))
    assert((-i to 2 sum) == 2)
    assert((-i to 2 sum) == (-i).to(2).sum)
  }
}
