import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.WrappedString

/**
  * See StringOps
  */
class StringTest extends AnyFunSuite {

  test("intersect") {
    val intersect = "abcde".toSeq.intersect("acef")

    // Why "Replace sameElements with equals" warning?
    assert(intersect.sameElements("ace"))
    assert(intersect != "ace")
    assert(intersect.isInstanceOf[WrappedString])
    assert(intersect.unwrap == "ace")
  }

  test("containsSlice") {
    assert("abc".containsSlice('a' to 'c'))
    assert(!"abc".containsSlice('a' to 'd'))
  }

  test("operators") {
    assert("abc" + "def" == "abcdef")
    assert("ab" * 3 == "ababab")
  }

  test("first and last") {
    assert("abc" (0) == 'a')
    assert("abc".last == 'c')
  }

  test("take, drop, takeRight, dropRight") {
    assert("abc".take(2) == "ab")
    assert("abc".drop(2) == "c")
    assert("abc".takeRight(2) == "bc")
    assert("abc".dropRight(2) == "a")
  }

  /**
    * https://docs.scala-lang.org/overviews/core/string-interpolation.html
    */
  test("string interpolation") {
    val name = "Sang"
    val age = 37
    assert(f"$name: ${age + 1}" == "Sang: 38")

    // f is typesafe
    // $$ -> $
    assert(f"$$10" == "$10")
    assert(f"$$$age" == "$37")
    assert(s"$name" == "Sang")

    // raw
    assert(raw"a\nb" == s"a\\nb")
  }
}
