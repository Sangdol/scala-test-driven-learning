import org.scalatest.funsuite.AnyFunSuite

class CaseClassTest extends AnyFunSuite {

  // https://docs.scala-lang.org/tour/case-classes.html
  test("Case Class") {
    case class Book(isbn: String)

    val b1 = Book("1")
    assert(b1.isbn == "1")

    val b2 = Book("1")
    assert(b1 == b2)

    val b3 = b2.copy(isbn = "2")
    assert(b3.isbn == "2")

    def bookGenerator: String => Book = isbn => Book(f"isbn: $isbn")

    assert(bookGenerator("123").isbn == "isbn: 123")

    def bookGenerator2(isbn: String): Book = {
      Book(f"ISBN: $isbn")
    }

    assert(bookGenerator2("111").isbn == "ISBN: 111")
  }

  test("Case Class default arguments") {
    case class Book1(title: String = "", year: Int)

    val b1 = Book1("b1", 2021)

    assert(b1.title == "b1")
    assert(b1.year == 2021)

    val b11 = Book1(year=2022)

    assert(b11.title == "")
    assert(b11.year == 2022)

    case class Book2(year: Int, title: String = "")

    val b2 = Book2(2021)

    assert(b2.year == 2021)
    assert(b2.title == "")
  }

  test("toString") {
    case class Book(title: String = "", year: Option[Int])

    val b1 = Book("hi", None)
    assert(b1.toString == "Book(hi,None)")

    val b2 = Book("hi", Some(1))
    assert(b2.toString == "Book(hi,Some(1))")
  }
}
