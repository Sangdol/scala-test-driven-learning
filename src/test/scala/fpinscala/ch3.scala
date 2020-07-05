package fpinscala

/**
 * Functional Programming in Scala
 */

import org.scalatest.funsuite.AnyFunSuite

sealed trait MyList[+A] // abstract class
case object Nil extends MyList[Nothing] // singleton class
case class Cons[+B](head: B, tail: MyList[B]) extends MyList[B]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ints: MyList[Double]): Double = ints match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function: it takes 0 or more arguments of type A (Seq)
  // https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // _*: spread

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Nil => sys.error("no more element")
    case Cons(_, xs) => xs
  }

  def setHead[A](list: MyList[A], element: A): MyList[A] = list match {
    case Nil => Cons(element, Nil)
    case Cons(_, xs) => Cons(element, xs)
  }

}

class ch3 extends AnyFunSuite {

  test("3.1") {
    def matchTest(list: MyList[Int]): Int =
      list match {
        case Cons(x, Cons(2, Cons(4, _))) => x // 1
        case Nil => 42 // 2
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
        case Cons(h, t) => h + MyList.sum(t) // 4
        case _ => 101 // 5
      }

    assert(matchTest(MyList(1,2,4,4,5)) == 1) // 1
    assert(matchTest(Nil) == 42) // 2
    assert(matchTest(MyList()) == 42) // 2
    assert(matchTest(MyList(1,2,3,4,5)) == 3) // 3
    assert(matchTest(MyList(2,3,4,4,5)) == 18) // 4
    assert(matchTest(MyList(2,3,4,4,5,6)) == 24) // 4
  }

  test("3.2") {
    assert(MyList.tail(MyList(1,2,3)) == MyList(2,3))
  }

  test("3.3") {
    assert(MyList.setHead(MyList(1,2), 3) == MyList(3,2))
    assert(MyList.setHead(Nil, 3) == MyList(3))
  }
}
