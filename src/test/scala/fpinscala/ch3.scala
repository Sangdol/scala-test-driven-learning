package fpinscala

/**
 * Functional Programming in Scala
 */

import org.scalatest.funsuite.AnyFunSuite

sealed trait List[+A] // abstract class
case object Nil extends List[Nothing] // singleton class
case class Cons[+B](head: B, tail: List[B]) extends List[B]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ints: List[Double]): Double = ints match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function: it takes 0 or more arguments of type A (Seq)
  // https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // _*: spread

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("no more element")
    case Cons(_, xs) => xs
  }

  @scala.annotation.tailrec
  def drop[A](list: List[A], n: Int): List[A] = list match {
    case list if n <= 0 => list  // pattern guard
    case Nil => Nil
    case Cons(x, xs) => drop(xs, n-1)
  }

  @scala.annotation.tailrec
  def dropFromBook[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => dropFromBook(t, n-1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def setHead[A](list: List[A], element: A): List[A] = list match {
    case Nil => Cons(element, Nil)
    case Cons(_, xs) => Cons(element, xs)
  }

}

class ch3 extends AnyFunSuite {

  test("3.1") {
    def matchTest(list: List[Int]): Int =
      list match {
        case Cons(x, Cons(2, Cons(4, _))) => x // 1
        case Nil => 42 // 2
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
        case Cons(h, t) => h + List.sum(t) // 4
        case _ => 101 // 5
      }

    assert(matchTest(List(1,2,4,4,5)) == 1) // 1
    assert(matchTest(Nil) == 42) // 2
    assert(matchTest(List()) == 42) // 2
    assert(matchTest(List(1,2,3,4,5)) == 3) // 3
    assert(matchTest(List(2,3,4,4,5)) == 18) // 4
    assert(matchTest(List(2,3,4,4,5,6)) == 24) // 4
  }

  test("3.2") {
    assert(List.tail(List(1,2,3)) == List(2,3))
  }

  test("3.3") {
    assert(List.setHead(List(1,2), 3) == List(3,2))
    assert(List.setHead(Nil, 3) == List(3))
  }

  test("3.4") {
    assert(List.drop(List(1,2,3), 2) == List(3))
    assert(List.drop(List(1), -1) == List(1))
  }

  test("3.5") {
    assert(List.dropWhile(List(1,2,3), (x: Int) => x < 3) == List(3))
  }

  test("3.6") {
    assert(List.init(List(1,2,3)) == List(1,2))
  }
}
