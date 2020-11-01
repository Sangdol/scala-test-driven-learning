package fpinscala

import org.scalatest.funsuite.AnyFunSuite

import scala.math.Ordering.Implicits._
import RNG._

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, r1) = rng.nextInt
    val (i2, r2) = r1.nextInt
    ((i1, i2), r2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i == Int.MinValue) (0, r)
    else (math.abs(i), r)
  }

  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    (if (i < 0) -(i+1) else i, r)
  }

  // 0~1(exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i % Int.MaxValue).toDouble / Int.MaxValue, r)
  }

  def double2(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = RNG.double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) = count match {
      case 0 => (l, rng)
      case _ =>
        val (i, r) = rng.nextInt
        go(count-1, r, i :: l)
    }

    go(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  val nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](s1: Rand[A], s2: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a1, r1) = s1(rng)
    val (a2, r2) = s2(r1)
    (f(a1, a2), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // (difficult - answer from the author))
  // Isn't acc a list? How can it be used as an argument of map2?
  //  No, acc is Rand[List[A]] and f is Rand[A].
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def go[B](fs: List[Rand[B]], as: List[B]): Rand[List[B]] = rng => {
        fs match {
          case Nil => unit(as)(rng)
          case f :: t =>
            val (a2, r2) = f(rng)
            go(t, a2 :: as)(r2)
        }
    }

    go(fs, Nil)(rng)
  }

  def intsViaSequence[A](count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def intsViaSequence2[A](count: Int): Rand[List[Int]] =
    sequence2(List.fill(count)(int))

}

// how to test it?
class ch6 extends AnyFunSuite {

  test("6.1") {
    assert(RNG.nonNegativeInt(Simple(1))._1 > 0)
    assert(RNG.nonNegativeInt2(Simple(1))._1 > 0)
  }

  test("6.2") {
    assert(RNG.double(Simple(1))._1 < 1)
    assert(RNG.double(Simple(1))._1 >= 0)
  }

  test("6.3") {
    assert(RNG.intDouble(Simple(1))._1 <= (Int.MaxValue, 1))
    assert(RNG.doubleInt(Simple(1))._1 <= (1, Int.MaxValue))
    assert(RNG.double3(Simple(1))._1 <= (1,1,1))
  }

  test("6.4") {
    assert(RNG.ints(3)(Simple(1))._1.size == 3)
  }

  test("nonNegativeEven") {
    assert(nonNegativeEven(Simple(1))._1 % 2 == 0)
  }

  test("6.5") {
    assert(doubleViaMap(Simple(1))._1 < 1)
  }

  test("6.6") {
    assert(randIntDouble(Simple(1))._1 <= (Int.MaxValue, 1))
    assert(randDoubleInt(Simple(1))._1 <= (1, Int.MaxValue))
  }

  test("6.7") {
    assert(intsViaSequence(3)(Simple(1))._1.size == 3)
    assert(intsViaSequence2(3)(Simple(1))._1.size == 3)
  }

}
