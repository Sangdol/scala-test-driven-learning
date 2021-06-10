import org.scalatest.funsuite.AnyFunSuite

class PatternMatchingTest extends AnyFunSuite {
  // https://docs.scala-lang.org/tour/pattern-matching.html
  test("Basic") {
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

  test("Binding variables @ ") {
    case class Name(firstName: String, lastName: String)
    case class Person(name: Name, age: Int)

    val sang = Person(Name("Sang", "Lee"), 38)
    val hj = Person(Name("hj", "Kim"), 37)

    for (person <- Seq(sang, hj)) {
      person match {
        case p @ Person(name, 38) =>
          assert(p == Person(Name("Sang", "Lee"), 38))
        case p @ Person(n @ Name(firstName, lastName), 37) => {
          assert(n.firstName == "hj")
          assert(lastName == "Kim")
        }
      }
    }
  }

  test("regex1") {
    def isAlphabetWord(word: String): Boolean = {
      val pattern = "([a-cA-C]+)".r
      word match {
        case pattern(c) => true
        case _          => false
      }
    }

    assert(isAlphabetWord("abC"))
    assert(isAlphabetWord("abc"))
    assert(!isAlphabetWord("abc`"))
  }

  test("regex2") {
    // https://www.scala-lang.org/api/2.12.5/scala/util/matching/Regex.html
    val date = raw"(\d{4})-(\d{2})-(\d{2})".r

    var year: String = ""
    var month: String = ""
    var day: String = ""

    "2021-06-10" match {
      case date(y, m, d) =>
        year = y
        month = m
        day = d
    }

    assert(f"$year-$month-$day" == "2021-06-10")
  }
}
