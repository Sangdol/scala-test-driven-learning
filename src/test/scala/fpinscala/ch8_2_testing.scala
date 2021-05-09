package fpinscala

import fpinscala.Prop.{FailedCase, SuccessCount, TestCases, forAll}
import fpinscala.RNG.nonNegativeInt
import org.scalatest.funsuite.AnyFunSuite

case class Prop(run: (TestCases, RNG) => Result) {

  // where is `max` from in the blue book solution?
  // What is Prop.check for?
  // Difficult
  def &&(p: Prop): Prop =
    Prop { (n, rng) =>
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case x      => x
      }
    }

  def ||(p: Prop): Prop =
    Prop { (n, rng) =>
      run(n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(n, rng)
        case x                 => x
      }
    }

  def tag(msg: String): Prop =
    Prop { (n, rng) =>
      run(n, rng) match {
        case Falsified(failure, successes) =>
          Falsified(msg + "\n" + failure, successes)
        case x => x
      }
    }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse { Passed }
    }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

// "It's nothing more than a non-strict Either." What does it mean by non-strict?
trait PropTrait {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def listOf[A](a: Gen[A]): Gen[List[A]]
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  // why the book doesn't have 'override'?
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  override def isFalsified: Boolean = true
}

// Covariant Type A occurs in invariant position
//case class SGen[+A](forSize: Int => Gen[A])
case class SGen[A](forSize: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)

  def mapFromBlue[B](f: A => B): SGen[B] =
    SGen(forSize andThen (_ map f))

  def map2[B, C](b: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen(s => forSize(s).map2(b.forSize(s))(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(s => forSize(s).flatMap(f(_).forSize(s)))

  // Is this a right signature?
  def flatMapFromBlue[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))

}

// How to extract a number from Gen?
//   Gen.sample.run(RNG.simple(n))...
// RNG itself can have it's own value. Why do we need a State?
//   We need a processed value rather than the direct value from RNG.
case class Gen[A](sample: State[RNG, A]) {

  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(b.sample)(f))

  def mapViaMap2[B](f: A => B): Gen[B] =
    map2(Gen.unit())((a, _) => f(a))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  // Difficult
  // What does it do?
  //   It generates `size` random numbers.
  // Why do we use flatMap here?
  //   Because flatMap can turn Gen[Int] to Gen[List[A]].
  // But flatMap will take A from this (Gen)? Where do we use the A?
  //   We generate `size` As.
  // But A will be provided as a value. Where do we use it?
  //   We don't need to explicitly use it. It'll be the first value of List[A].
  // How to turn A to List[A]?
  //   State.sequence
  // How to get int size from Gen size?
  //   size.flatMap
  // Why is it Gen[Int] instead of Int?
  //   We want to see how dependent Gen works.
  // What does Gen[Int] and Gen[List[A]] mean?
  //   Gen[Int]: it generates an int.
  //   Gen[List[A]]: it generates a list of A.
  // Who decide the value of A?
  //   RNG
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State { rng =>
      val (n, nextRng) = nonNegativeInt(rng)
      val res = n % (stopExclusive - start) + start
      (res, nextRng)
    })

  def tuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val s = State { rng: RNG =>
      val (n, nextRng) = nonNegativeInt(rng)
      val res = n % (stopExclusive - start) + start
      (res, nextRng)
    }

    Gen(s.map2(s)((a, b) => (a, b)))
  }

  def tuple2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((_, _))

  def tuple3(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val c = choose(start, stopExclusive)
    c.map2(c)((_, _))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  // answer
  def unit2[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(rng => {
      val (n, nextRng) = nonNegativeInt(rng)
      (n % 2 == 0, nextRng)
    }))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // w1, w2 = turnToInt(g1w, g2w)
  // choose(w1, w1+w2).flatMap(a => if (a < w1) g1 else g2)
  // or
  // choose(0, intMax) - scale down to w1+w2
  // or
  // random.gen(w1+w2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (ga1, w1) = g1
    val (ga2, w2) = g2

    // This is not purely functional
    // because this breaks referential transparency.
    val r = util.Random.nextDouble

    if ((w1 + w2) * r < w1) ga1 else ga2
  }

  def weighted2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    choose(Int.MinValue, Int.MaxValue).flatMap { n =>
      val (ga1, w1) = g1
      val (ga2, w2) = g2
      val scaled = n * (w1 + w2) / (Int.MaxValue - Int.MinValue)

      if (scaled < w1) ga1 else ga2
    }

  // from the blue book
  def weighted3[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    // ABS?
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    // The code in the book is a bit wrong. (returning g1._1.sample)
    // Should we take the value from RNG?
    Gen(State(RNG.double)).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }

  def chooseDouble(start: Double, stopExclusive: Double): Gen[Double] =
    Gen(State { rng =>
      val (n, nextRng) = nonNegativeInt(rng)
      val res = n % (stopExclusive - start) + start
      (res, nextRng)
    })

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
      def &&(p: Prop): Prop =
        new Prop {
          def check: Boolean = this.check && p.check
        }
    }
  }

  test("8.4") {
    val rng = RNG.Simple(seed = 1)
    val random = Gen.choose(1, 3).sample.run(rng)._1

    assert(random == 1 || random == 2)
  }

  test("8.5") {
    val rng = RNG.Simple(seed = 1)
    assert(Gen.unit(1).sample.run(rng)._1 == 1)
    assert(Gen.boolean.sample.run(rng)._1)

    val sample: State[RNG, String] = State(rng => {
      val (n, nextRng) = rng.nextInt
      (n.toString, nextRng)
    })
    val gen = Gen(sample)

    assert(
      Gen.listOfN(3, gen).sample.run(rng)._1 ==
        List("-549383847", "-1151252339", "384748")
    )

    assert(
      Gen.listOfN2(3, gen).sample.run(rng)._1 ==
        List("384748", "-1151252339", "-549383847")
    )
  }

  test("Primitive vs. derived") {
    // If we can generate a single Int in some range,
    // do we need a new primitive to generate
    // an (Int,Int) pair in some range?
    //   (start, stop) -> (int, int)
    val rng = RNG.Simple(seed = 1)
    val randomPair = Gen.tuple(1, 5).sample.run(rng)._1

    assert(randomPair == (1, 4))

    val randomPair2 = Gen.tuple2(1, 100).sample.run(rng)._1
    val randomPair3 = Gen.tuple3(1, 100).sample.run(rng)._1

    assert(randomPair2 == randomPair3)

    // Can we produce a Gen[Option[A]] from a Gen[A]? Yes. Map.
    // What about a Gen[A] from a Gen[Option[A]]?
    //   No, because an Option might not have a value.
    val genOption = Gen.unit(1).map(a => Option(a))

    assert(genOption.sample.run(rng)._1.get == 1)

    // Can we generate strings somehow using our existing primitives?
    //   Yes. Map.
    val genStr = Gen.unit(1).map(a => a.toString)

    assert(genStr.sample.run(rng)._1 == "1")
  }

  test("8.6") {
    val sample: State[RNG, String] = State(rng => {
      val (n, nextRng) = rng.nextInt
      (n.toString, nextRng)
    })
    val gen = Gen(sample)

    val rng = RNG.Simple(seed = 1)

    assert(
      gen.listOfN(Gen.unit(3)).sample.run(rng)._1 ==
        List("-549383847", "-1151252339", "384748")
    )
  }

  test("8.7") {
    val gen1 = Gen(State.unit(1))
    val gen2 = Gen(State.unit(2))

    val rng = RNG.Simple(seed = 1)

    val union = Gen.union(gen1, gen2).sample.run(rng)._1
    assert(union == 1 || union == 2)
  }

  test("8.8") {
    val gen1 = Gen(State.unit(1))
    val gen2 = Gen(State.unit(2))

    val rng = RNG.Simple(seed = 1)

    val union = Gen.weighted((gen1, 0.0), (gen2, 0.2)).sample.run(rng)._1
    assert(union == 2)
  }

  test("8.9") {
    val gen1 = Gen(State.unit(1))
    val gen2 = Gen(State.unit(2))

    val rng = RNG.Simple(seed = 1)

    val prop = forAll(gen1)(_ > 0) && forAll(gen2)(_ > 0)

    assert(prop.run(2, rng) == Passed)

    val prop2 = forAll(gen1)(_ < 0) && forAll(gen2)(_ < 0)
    assert(prop2.run(2, rng) == Falsified("1", 0))

    val prop3 = forAll(gen1)(_ < 0) || forAll(gen2)(_ < 0)
    assert(prop3.run(2, rng) == Falsified("1\n2", 0))
  }

  test("8.10") {
    val gen = Gen(State.unit(1))

    assert(gen.unsized.getClass.getSimpleName == "SGen")
  }

  test("8.11") {
    val sgen1 = Gen(State.unit(1)).unsized
    val rng = RNG.Simple(seed = 1)
    val ANY = 0

    // map
    assert(sgen1.map(_ * 2).forSize(ANY).sample.run(rng)._1 == 2)

    // map2
    val sgen2 = Gen(State.unit(2)).unsized
    assert(sgen1.map2(sgen2)(_ * _).forSize(ANY).sample.run(rng)._1 == 2)

    // flatMap
    assert(
      sgen1
        .flatMap(n => Gen(State.unit(n * 2)).unsized)
        .forSize(ANY)
        .sample
        .run(rng)
        ._1 == 2
    )

    // flatMap from Blue book
    assert(
      sgen1
        .flatMapFromBlue(n => Gen(State.unit(2)))
        .forSize(ANY)
        .sample
        .run(rng)
        ._1 == 2
    )
  }

}
