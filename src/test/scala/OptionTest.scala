import org.scalatest.funsuite.AnyFunSuite

class OptionTest extends AnyFunSuite {

  // https://alvinalexander.com/scala/how-use-fold-scala-option-some-none-syntax/
  test("fold and getOrElse") {
    def f(i: Int) = i + 1

    val two = Some(2)
    val DEFAULT = 100

    assert(two.fold(DEFAULT)(f) == 3)
    assert(two.fold {
      val a = 1
      val b = 2
      a + b
    }(f) == 3)
    assert(two.map(f).getOrElse(DEFAULT) == 3)
  }

  test("get") {
    assertThrows[Exception](None.get)
  }

  test("option empty") {
    val none = Option.empty

    assert(none.getClass.toString == "class scala.None$")

    // This is needed sometimes...
    // https://stackoverflow.com/a/24797748/524588
    val str = Option.empty[String]

    assert(str.getClass.toString == "class scala.None$")
  }

  test("orElse, getOrElse") {
    val two = Some(2)
    val someTwo = two.orElse(Some(3))
    val stillTwo = two.getOrElse(3)

    assert(someTwo == Some(2))
    assert(stillTwo == 2)
  }

  test("many options") {
    val o1 = Some(1)
    val o2 = Option.empty[Int]
    val o3 = Some(3)

    val partialSum = (o1, o2, o3) match {
      case (Some(one), Some(two), Some(three)) => one + two + three
      // how could I put 'None' here?
      // pattern type is incompatible with expected type;
      //  [error]  found   : None.type
      //  [error]  required: Some[Int]
      //case (Some(one), Some(two), _) => one + two
      case (Some(one), Some(two), _)      => one + two
      case (Some(one), None, Some(three)) => one + three
      case _                              => 0
    }

    assert(partialSum == 4)
  }

  test("option") {
    val some = Some(1)
    val some2 = Some(2)

    val s = for {
      s1 <- some
      s2 <- some2
    } yield s1 + s2

    assert(s == Some(3))

    val s2 = some.flatMap(s1 => some2 map (s2 => s1 + s2))

    assert(s2 == Some(3))

    val some3: Option[Int] = None

    // None makes all result None
    val sNone = for {
      s1 <- some
      s2 <- some2
      s3 <- some3
    } yield s1 + s2 + s3

    assert(sNone == None)

    val s2None = some.flatMap(s1 => some2.flatMap(s2 => some3 map (s3 => s1 + s2 + s3)))

    assert(s2None == None)

    // flatten
    val ss = Seq(some, some2, some3)

    assert(ss.flatten == Seq(1, 2))
    assert(ss.flatten.sum == 3)

    // flatten is the same as
    assert(ss.flatMap(s => s) == Seq(1, 2))
  }

  test("option pattern matching") {
    // pattern matching

    val results: Seq[Option[Int]] = Vector(Some(10), None, Some(20))

    val filtered = for {
      Some(r) <- results
    } yield (r * 2)

    assert(filtered == Seq(20, 40))
  }
}
