package fpinscala

import org.scalatest.funsuite.AnyFunSuite
import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Option(h())
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def toList2: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, Nil).reverse
  }

  def toList3: List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def take(n: Int): Stream[A] = {
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
    }
  }

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) =>
      if (n > 1) t().drop(n-1)
      else t()
  }

  def drop2(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop2(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exist(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exist(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exist2(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>  {
//      This evaluates "b" and change the order of execution.
//      (similar to heisenbug)
//      println(a, b)
//
//      This is okay.
//      println(a)
      if (p(h)) cons(h, t) else Empty
    })

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Option(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  // difficult to find out the spec..
  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((h, t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))
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

  test("cons") {
    assert(List(1) == cons(1, Empty).toList)

  }

  test("5.1") {
    assert(Option(1) == Stream(1).headOption)
    assert(Stream().headOption.isEmpty)
    assert(List(1,2,3) == Stream(1,2,3).toList)

    assert(List(1,2,3) == Stream(1,2,3).toList2)
    assert(List(1,2,3) == Stream(1,2,3).toList3)
  }

  test("5.2") {
    assert(List(1,2) == Stream(1,2,3).take(2).toList)
    assert(List(1,2) == Stream(1,2,3).take2(2).toList)

    // drop
    assert(List() == Stream().drop(1).toList)
    assert(List() == Stream(1).drop(1).toList)
    assert(List(2) == Stream(1,2).drop(1).toList)
    assert(List(3) == Stream(1,2,3).drop(2).toList)

    assert(List() == Stream().drop2(1).toList)
    assert(List() == Stream(1).drop2(1).toList)
    assert(List(2) == Stream(1,2).drop2(1).toList)
    assert(List(3) == Stream(1,2,3).drop2(2).toList)
  }

  test("5.3") {
    assert(List() == Stream(1,2).takeWhile(_ > 2).toList)
    assert(List() == Stream(1,2,3,4).takeWhile(_ > 2).toList)
    assert(List(3,4) == Stream(3,4,1,2,3,4).takeWhile(_ > 2).toList)
  }

  test("exist") {
    assert(Stream(1,2).exist(_ == 1))
    assert(!Stream(1,2).exist(_ == 3))

    assert(Stream(1,2).exist2(_ == 1))
    assert(!Stream(1,2).exist2(_ == 3))
  }

  test("5.4") {
    assert(Stream(1,2).forAll(_ > 0))
    assert(!Stream(0,1,2,3).forAll(_ > 2))
  }

  test("5.5") {
    // ducking difficult to understand
    assert(List() == Stream(1,2).takeWhile2(_ > 2).toList)
    assert(List() == Stream(1,2,3,4).takeWhile2(_ > 2).toList)
    assert(List(3,4) == Stream(3,4,1,2,3,4).takeWhile2(_ > 2).toList)
    assert(List(4,3) == Stream(4,3,2,1).takeWhile2(_ > 2).toList)
  }

  test("5.6") {
    assert(Option(1) == Stream(1).headOption2)
    assert(Stream().headOption2.isEmpty)
  }

  test("5.7") {
    assert(List(2,4) == Stream(1,2).map(_ * 2).toList)
    assert(List(2) == Stream(1,2,3).filter(_ % 2 == 0).toList)
    assert(List(1,2) == Stream(1).append(Stream(2)).toList)
    assert(List(2,4) == Stream(1,2).flatMap(s => Stream(s*2)).toList)
  }
}
