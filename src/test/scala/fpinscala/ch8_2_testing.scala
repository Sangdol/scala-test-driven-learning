package fpinscala

import fpinscala.Prop.{FailedCase, SuccessCount}
import fpinscala.RNG.nonNegativeInt
import org.scalatest.funsuite.AnyFunSuite

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount),SuccessCount]
}


// How to extract a number from Gen?
//   Gen.sample.run(RNG.simple(n))...
// RNG itself can have it's own value. Why do we need a State?
//   We need a processed value rather than the direct value from RNG.
case class Gen[A](sample: State[RNG,A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State { rng =>
      val (n, nextRng) = nonNegativeInt(rng)
      val res = n % (stopExclusive - start) + start
      (res, nextRng)
    })

  def tuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    {
      val s = State { rng: RNG =>
        val (n, nextRng) = nonNegativeInt(rng)
        val res = n % (stopExclusive - start) + start
        (res, nextRng)
      }

      Gen(s.map2(s)((a, b) => (a, b)))
    }

  def tuple2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    map2(choose(start, stopExclusive), choose(start, stopExclusive))((_,_))

  def tuple3(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val c = choose(start, stopExclusive)
    map2(c, c)((_, _))
  }

  def map2[A,B,C](a: Gen[A], b: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(a.sample.map2(b.sample)(f))

//  def flatMap[A,B](g: Gen[A])(f: A => Gen[B]): Gen[B] = ???

  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  // answer
  def unit2[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(rng => {
      val (n, nextRng) = nonNegativeInt(rng)
      (n % 2 == 0, nextRng)
    }))

  // Can A be a type other than Int when RNG is used?
  //   That's why we have `g`.
  // A primitive solution.
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State(rng => {
      var nextRng = rng
      var list = List[A]()

      for (_ <- 1 to n) {
        val (a, n) = g.sample.run(nextRng)
        nextRng = n
        list = a :: list
      }

      (list, nextRng)
    }))

  // from the blue book
  // How are the states connected?
  //   rng is injected anyway and sequence pass accumulator to the next element.
  // A derived solution.
  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
}

trait GenTrait[T] {
  def listOf[A](a: Gen[A]): Gen[List[A]]
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop
}

class ch8_2_testing extends AnyFunSuite {
  test("8.1") {
    // Reversing a list and summing it should give the same result as
    // summing the original, nonreversed list.
    //  => sum(list.reverse) == sum(list)

    // What should the sum be if all elements of the list are the same value?
    //  => sum([11] * 10) == 11 * 10
    //  => How could I generate this input?

    // Can you think of other properties?
    //  => sum(range(n + 1)) == n(n+1)/2

    // more
    //  sum(emptyList) == 0
    //  sum([a,b,c]) == sum([a]) + sum([b,c])
  }

  test("8.2") {
    // What properties specify a function that finds the maximum of a List[Int]?
    //  => n <= max(list)

    // more
    //  max([n]) == n
    //  max(list) is in list
    //  max(emptyList) => null or exception

    // more
    //  lastElement(sort(list)) == max(list)
    //  max(list1 ++ list2) == max([max(list1), max(list2)])
    //  max(list :: [n]) == max([max(list), n])
    //  max(tail(list)) == max()
  }

  test("8.3") {
    trait Prop {
      def check: Boolean
      def assert: Unit = if (!check) throw new AssertionError()

      // 8.3 - a new way to implement laziness
      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = this.check && p.check
      }
    }
  }

  test("8.4") {
    val rng = RNG.Simple(seed=1)
    val random = Gen.choose(1, 3).sample.run(rng)._1

    assert(random == 1 || random == 2)
  }

  test("8.5") {
    val rng = RNG.Simple(seed=1)
    assert(Gen.unit(1).sample.run(rng)._1 == 1)
    assert(Gen.boolean.sample.run(rng)._1)

    val sample: State[RNG, String] = State(rng => {
      val (n, nextRng) = rng.nextInt
      (n.toString, nextRng)
    })
    val gen = Gen(sample)

    assert(Gen.listOfN(3, gen).sample.run(rng)._1 ==
      List("-549383847", "-1151252339", "384748"))

    assert(Gen.listOfN2(3, gen).sample.run(rng)._1 ==
      List("384748", "-1151252339", "-549383847"))
  }

  test("Primitive vs. derived") {
    // If we can generate a single Int in some range,
    // do we need a new primitive to generate
    // an (Int,Int) pair in some range?
    //   (start, stop) -> (int, int)
    val rng = RNG.Simple(seed=1)
    val randomPair = Gen.tuple(1, 5)
      .sample.run(rng)._1

    assert(randomPair == (1, 4))

    val randomPair2 = Gen.tuple2(1, 100)
      .sample.run(rng)._1
    val randomPair3 = Gen.tuple3(1, 100)
      .sample.run(rng)._1

    assert(randomPair2 == randomPair3)

    // Can we produce a Gen[Option[A]] from a Gen[A]?
    // What about a Gen[A] from a Gen[Option[A]]?
  }

}
