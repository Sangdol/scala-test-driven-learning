import org.scalatest.funsuite.AnyFunSuite

import scala.util._

class EnumerationTest extends AnyFunSuite {

  test("string to enum") {
    // https://stackoverflow.com/questions/33593525/scala-safe-way-of-converting-string-to-enumeration-value
    object Color extends Enumeration {
      type Color = Value
      val RED = Value(1, "red")
      val BLUE = Value(2, "blue")

      def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
    }

    assert(Color.withName("red") == Color.RED)
    assertThrows[NoSuchElementException](Color.withName("not-found"))

    assert(Color.withNameOpt("red") == Some(Color.RED))
  }

  test("enum id") {
    object Color extends Enumeration {
      type Color = Value
      val RED, BLUE = Value
    }

    assert(Color.RED.id == 0)
    assert(Color.BLUE.id == 1)

    object Color2 extends Enumeration {
      type Color = Value
      val RED = Value(1, "red")
      val BLUE = Value(2, "blue")
    }
    assert(Color2.RED.id == 1)
    assert(Color2.BLUE.id == 2)
  }

  test("get name") {
    object Color extends Enumeration {
      type Color = Value

      val RED = Value("red")
    }

    assert(Color.RED.toString == "red")
  }

  test("method of value") {
    // https://stackoverflow.com/a/19080686/524588
    object Color extends Enumeration {
      abstract class ColorValue(var name: String) extends Val(name) {
        def m: Int
      }

      val RED = new ColorValue("red") {
        def m = 0
      }
      val BLUE = new ColorValue("blue") {
        def m = 1
      }
    }

    assert(Color.RED.m == 0)
    assert(Color.BLUE.m == 1)
  }

  /**
    * https://www.scala-lang.org/api/current/scala/Enumeration.html
    */
  test("Enum with case class val") {

    object Multiple extends Enumeration {
      // Why private?
      // to hide the constructor / only without new
      // https://stackoverflow.com/questions/20030826/scala-case-class-private-constructor-but-public-apply-method
      final case class MultipleVal private (mid: String, name: String) extends super.Val(mid)

      type Multiple = MultipleVal

      val sang: Multiple.MultipleVal = MultipleVal("1", "sang")
      val hj: Multiple.MultipleVal = MultipleVal("2", "hj")
    }

    val ids = Multiple.values.map(_.toString)
    assert(ids == Set("1", "2"))

    val collectedIds = Multiple.values.collect {
      case Multiple.MultipleVal(id, _) => id
    }
    assert(collectedIds == Set("1", "2"))

    assert(Multiple.withName("1") == Multiple.sang)
    assert(Multiple.withName("1").asInstanceOf[Multiple.MultipleVal] == Multiple.sang)
  }

  test("Case class as enum") {

    /**
      * https://stackoverflow.com/questions/1898932/case-objects-vs-enumerations-in-scala
      *
      * Pros
      * - Easily extensible (in terms of adding functions)
      * - Pattern matching and a compiler check for a full specification
      * - Natural supports multiple arguments
      *
      * Cons
      * - More code to write
      * - No iterator over all instances
      * - No withName()
      */
    sealed trait Color { def name: String }
    case object RED extends Color { def name = "red" }
    case object BLUE extends Color { def name = "blue" }

    val c: Color = RED
    val a = c match {
      case RED  => "r"
      case BLUE => "b"
    }

    assert(a == "r")
  }

  test("option string to option enum") {
    val red = Some("RED")
    val black = Some("BLACK")

    object Color extends Enumeration {
      type Color = Value
      val RED, BLUE = Value
    }

    val eRed = red
      .flatMap { r =>
        Try(Color.withName(r)).toOption
      }
      .getOrElse(None)

    assert(eRed == Color.RED)

    val eBlack = black
      .flatMap { r =>
        Try(Color.withName(r)).toOption
      }
      .getOrElse(None)

    assert(eBlack == None)
  }

  test("enum should use its value when it's used in a string") {
    object Color extends Enumeration {
      type Color = Value
      val RED = Value(1, "red")
      val BLUE = Value(2, "blue")
    }

    assert(s"${Color.RED}" == "red")
  }
}
