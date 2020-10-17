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
      case _ => {
        val (i, r) = rng.nextInt
        go(count-1, r, i :: l)
      }
    }

    go(count, rng, Nil)
  }

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

}
