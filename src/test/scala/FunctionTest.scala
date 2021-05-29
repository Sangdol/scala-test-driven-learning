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
    def valueAtOneQuater(f: Double => Double) = f(0.25)

    assert(valueAtOneQuater((x: Double) => x * 2) == 0.5)
    assert(valueAtOneQuater((x: Double) => x * 2) == 0.5)
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
    assert(sum(List(1, 2, 3): _*) == 6)

    def recursiveSum(args: Int*): Int = {
      if (args.isEmpty) 0
      else args.head + recursiveSum(args.tail: _*)
    }

    assert(recursiveSum(1 to 3: _*) == 6)
    assert(recursiveSum(1, 2, 3) == 6)
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

    val explicitFun: Double => Double = math.ceil
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
    // What is the class of this method?
    //   FunctionTest
    def add1(n: Int): Int = n + 1

    // You need to put '_' to make it function
    val f = add1 _

    assert(f(1) == 2)

    // As this is a function you can assign it to a variable right away.
    // Why is it designed like this?
    // --> This is a method that has a static value which is a function
    //     This method need to be called without ().
    //     https://docs.scala-lang.org/tour/basics.html#methods
    def add2: Int => Int = n => n + 2
    assert(add2(1) == 3)

    val f2 = add2
    assert(f2(1) == 3)

    // This is a normal way to define a function.
    val add2_1: Int => Int = (n: Int) => n + 2
    val f2_1 = add2_1

    assert(f2_1(1) == 3)

    // multiline method
    def add3(n: Int): Int = {
      n + 3
    }

    val f3 = add3 _
    assert(f3(1) == 4)

    // multiline function
    val add4: Int => Int = (n: Int) => {
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

  test("lambda styles") {
    // https://www.scala-exercises.org/std_lib/higher_order_functions
    def lambda = { x: Int => x + 1 }
    def lambda2 = (x: Int) => x + 2
    val lambda3 = (x: Int) => x + 3

    // A function is an object with an `apply` under the hood
    val lambda4 = new Function1[Int, Int] {
      override def apply(v1: Int): Int = v1 + 4
    }

    val lambda5 = new (Int => Int) {
      override def apply(v1: Int): Int = v1 + 5
    }

    def lambda6(x: Int) = x + 6

    assert(lambda(0) == 1)
    assert(lambda2(0) == 2)
    assert(lambda3(0) == 3)
    assert(lambda4(0) == 4)
    assert(lambda5(0) == 5)
    assert(lambda6(0) == 6)
  }

  // partial: not defined for all possible input
  test("PartialFunction") {
    // 1. only case can be specified
    // 2. only curly braces (no parentheses)
    // 3. MatchError for a not handled input
    val pf1: PartialFunction[Any, String] = { case s: String => "YES" }
    val pf2: PartialFunction[Any, String] = { case s: Double => "YES" }

    val pf = pf1 orElse pf2

    // pf1
    assert(pf1.isDefinedAt("str"))
    assert(pf1("str") == "YES")
    assertThrows[MatchError](pf1(1.0))

    // pf2
    assert(!pf2.isDefinedAt("str"))
    assert(pf2(1.0) == "YES")
    assertThrows[MatchError](pf2(1))
    assertThrows[MatchError](pf2("str"))

    // pf
    assert(pf.isDefinedAt("str"))
    assert(pf(1.0) == "YES")
    assert(pf("str") == "YES")
  }
}
