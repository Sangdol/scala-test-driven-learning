package algorithm

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class DecimalToBinary extends AnyFunSuite {

  def decimalToBinary(i: Int): String = {
    if (i == 0) return "0"

    var res = ""
    var n: Int = i

    while (n != 1) {
      val remainder = n % 2
      n /= 2
      res = s"$remainder$res"
    }

    s"1$res"
  }

  test("dtob") {
    assert(decimalToBinary(0) == "0")
    assert(decimalToBinary(1) == "1")
    assert(decimalToBinary(2) == "10")
    assert(decimalToBinary(3) == "11")
    assert(decimalToBinary(10) == "1010")
  }

  def decimalToBinaryRecur(i: Int): String = {
    if (i == 0) return "0"

    @tailrec
    def go(n: Int, res: String): String = {
      if (n == 1) s"1$res"
      else go(n / 2, s"${n % 2}$res")
    }

    go(i, "")
  }

  test("dtob recur") {
    assert(decimalToBinaryRecur(0) == "0")
    assert(decimalToBinaryRecur(1) == "1")
    assert(decimalToBinaryRecur(2) == "10")
    assert(decimalToBinaryRecur(3) == "11")
    assert(decimalToBinaryRecur(10) == "1010")
  }

  def decimalToBinaryIter(i: Int): String = {
    if (i == 0) return "0"

    Iterator
      .iterate(i)(_ / 2)
      .takeWhile(_ > 0)
      .map(_ % 2)
      .mkString("")
      .reverse
  }

  test("dtob iter") {
    assert(decimalToBinaryIter(0) == "0")
    assert(decimalToBinaryIter(1) == "1")
    assert(decimalToBinaryIter(2) == "10")
    assert(decimalToBinaryIter(3) == "11")
    assert(decimalToBinaryIter(10) == "1010")
  }
}
