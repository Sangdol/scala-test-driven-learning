import org.scalatest.funsuite.AnyFunSuite

class FunctionTest extends AnyFunSuite {

  test("Anonymous function") {
    assert(((x: Int) => x * x)(3) === 9)

    // underscore (positional match)
    assert((1 to 3).map(_ * 2) === Array(2, 4, 6))
    assert((1 to 3).reduceLeft(_ / _) === 0)

    // no underscore possible in this case
    assert((1 to 3).map(x => x * x) === Array(1, 4, 9))

    // block style
    assert((1 to 3).map { x =>
      val y = x * 2
      y
    } === Array(2, 4, 6))

    // pipeline style (infix notation)
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

  test("Parameter inference") {
    def valueAtOneQuater(f: (Double) => Double) = f(0.25)

    assert(valueAtOneQuater((x: Double) => x * 2) == 0.5)
    assert(valueAtOneQuater((x) => x * 2) == 0.5)
    assert(valueAtOneQuater(x => x * 2) == 0.5)
    assert(valueAtOneQuater(_ * 2) == 0.5)
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
    def decorate(str: String, left: String = "[", right: String = "]") = {
      left + str + right
    }

    assert(decorate("abc") == "[abc]")
    assert(decorate("abc", "<", ">") == "<abc>")
    assert(decorate(str = "abc", right = "<", left = ">") == ">abc<")
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

  test("Function as a value") {
    // In Scala, you cannot manipulate methods, only functions.

    // Package methods
    // _ turns the ceil method into function
    val fun = math.ceil _
    assert(fun(1.1) == 2)

    val explicitFun: (Double) => Double = math.ceil
    assert(explicitFun(1.1) == 2)

    // Class methods
    val charAt = (_: String).charAt(_: Int)
    assert(charAt("abc", 1) == 'b')

    val charAt2: (String, Int) => Char = _.charAt(_)
    assert(charAt2("abc", 1) == 'b')
  }

  test("Methods are not functions") {
    // This is a method. A method is not a value.
    // So you cannot assign this to a variable.
    def add1(n: Int): Int = n + 1

    // You need to put '_' to make it function
    val f = add1 _

    assert(f(1) == 2)

    // As this is a function you can assign it to a variable right away.
    // Why is it designed like this?
    // --> This is a method that has a static value which is a function
    //     This method need to be called without ().
    //     https://docs.scala-lang.org/tour/basics.html#methods
    def add2: (Int) => Int = (n) => n + 2
    val f2 = add2
    assert(f2(1) == 3)

    // This is a normal way to define a function.
    val add2_1: (Int) => Int = (n) => n + 2
    val f2_1 = add2_1

    assert(f2_1(1) == 3)

    // multiline method
    def add3(n: Int): Int = {
      n + 3
    }

    val f3 = add3 _
    assert(f3(1) == 4)

    // multiline function
    val add4: (Int) => Int = (n) => {
      n + 4
    }

    val f4 = add4
    assert(f4(1) == 5)
  }

  test("Closure") {
    def mulBy(factor: Double) = (x: Double) => factor * x

    val tens = mulBy(10)
    assert(tens(3) == 30)
  }

  test("Currying") {
    // This is function.
    val mulCurrying = (a: Int) => (b: Int) => a * b

    assert(mulCurrying(2)(3) == 6)

    // This is method. Why? It's just syntax.
    def mulCurryingBetter(a: Int)(b: Int) = a * b

    assert(mulCurryingBetter(2)(3) == 6)

    val fun = mulCurryingBetter _

    assert(fun(3)(4) == 12)
  }

  test("apply") {
    val one = () => 1

    assert(one.apply() == 1)

    def two(): Int = 2

    assert((two _).apply() == 2)
  }

  /**
    * https://twitter.github.io/scala_school/pattern-matching-and-functional-composition.html
    */
  test("composition") {
    def f(s: String) = s"f($s)"
    def g(s: String) = s"g($s)"

    assert((f _ compose g)("hello") == "f(g(hello))")
    assert((f _ andThen g)("hello") == "g(f(hello))")
  }

  test("Placeholder syntax (underscore)") {
    val as = List(1, 2, 3)
    assert(as.filter(_ > 1) == as.filter(a => a > 1))

    val f = (_: Int) + (_: Int)
    assert(f(2, 3) == 5)
  }
}
