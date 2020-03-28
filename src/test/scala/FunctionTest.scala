import org.scalatest.funsuite.AnyFunSuite

class FunctionTest extends AnyFunSuite {

  test("Anonymous function") {
    assert(((x: Int) => x * x) (3) === 9)

    // underscore
    assert((1 to 3).map(_ * 2) === Array(2, 4, 6)) // positional match
    assert((1 to 3).reduceLeft(_ / _) === 0) // positional match
    assert((1 to 3).map(x => x * x) === Array(1, 4, 9)) // no underscore possible in this case

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

    assert(((1 to 3) filter {
      _ % 2 == 0
    } map {
      _ * 2
    }) === Array(4))
  }

  test("Function declaration") {
    // return type is not needed unless it's a recursive function
    def abs(n: Int) = if (n > 0) n else -n

    assert(abs(-1) == 1)

    // Scala compiler can't infer the type of "n * fac(n - 1)".
    def fac(n: Int): Int = if (n <= 1) 1 else n * fac(n - 1)

    assert(fac(3) == 6)
  }

  test("Default and named arguments") {
    def decorate(str: String, left: String = "[", right: String = "]")  = {
      left + str + right
    }

    assert(decorate("abc") == "[abc]")
    assert(decorate("abc", "<", ">") == "<abc>")
    assert(decorate(str="abc", right="<", left=">") == ">abc<")
  }

  test("Variable arguments") {
    def sum(args: Int*) = {
      var res = 0
      for (i <- args) res += i
      res
    }

    assert(sum(1, 2, 3) == 6)

    // _*: Range to argument sequence
    assert(sum(1 to 3: _*) == 6)

    def recursiveSum(args: Int*): Int = {
      if (args.isEmpty) 0
      else args.head + recursiveSum(args.tail: _*)
    }

    assert(recursiveSum(1 to 3: _*) == 6)
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

  test("Procedure") {
    // Procedures don't return any value (aka returns Unit).
    def proc(n: Int) {
      assert(n == 10)
    }

    proc(10)
  }

  test("Closure") {
    def mulBy(factor: Double) = (x: Double) => factor * x

    val tens = mulBy(10)
    assert(tens(3) == 30)
  }

}
