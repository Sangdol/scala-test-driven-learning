package fpinscala

import org.scalatest.funsuite.AnyFunSuite

sealed trait Stream[+A] {

  def optionHead: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Option(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => (h() :: t().toList)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl

    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else Cons(() => as.head, () => apply(as.tail: _*))

}

class ch5 extends AnyFunSuite {

  test("if2 and lazy") {
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
      if (cond) onTrue() else onFalse()

    // "() => A" is short for "Function0[A]"
    // the unevaluated form of an expression is called "thunk"
    assert(1 == if2(cond = true, () => 1, () => 2))

    def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
      if (cond) onTrue else onFalse

    assert(1 == if3(cond = true, 1, 2))

    // "i" is in fact a function call so it evaluates twice.
    def maybeTwice(b: Boolean, i: => Int): Int = if (b) i + i else 0

    var counter = 0
    assert(22 == maybeTwice(b = true, {counter+=1; 11}))
    assert(counter == 2)  // double evaluation

    def maybeTwice2(b: Boolean, i: => Int): Int = {
      lazy val j = i  // lazy is not only lazy to evaluate but also caches the result
      if (b) j + j else 0
    }

    var counter2 = 0
    assert(24 == maybeTwice2(b = true, {counter2+=1; 12}))
    assert(counter2 == 1)  // double evaluation
  }

  test("5.1") {
    assert(Option(1) == Stream(1).optionHead)
    assert(List(1, 2, 3) == Stream(1, 2, 3).toList)
  }

}
