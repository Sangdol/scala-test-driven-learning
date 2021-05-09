import org.scalatest.funsuite.AnyFunSuite

class StyleTest extends AnyFunSuite {
  test("infix vs. dot notation") {
    // https://docs.scala-lang.org/style/method-invocation.html
    // https://stackoverflow.com/questions/10233227/scala-infix-vs-dot-notation
    // infix notation could be confusing when it takes chaining function or !.

    val as = List(1, 2, 3)

    assert(as.filter(_ > 1) == List(2, 3))

    // It doesn't work without ().
    assert((as filter { _ > 1 }) == List(2, 3))
  }
}
