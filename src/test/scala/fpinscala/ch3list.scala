package fpinscala

/**
 * Functional Programming in Scala
 */

import org.scalatest.funsuite.AnyFunSuite

sealed trait MyList[+A] // abstract class
case object MyNil extends MyList[Nothing] // singleton class
case class MyCons[+B](head: B, tail: MyList[B]) extends MyList[B]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x,xs) => x + sum(xs)
  }

  def product(ints: MyList[Double]): Double = ints match {
    case MyNil => 1
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }

  // variadic function: it takes 0 or more arguments of type A (Seq)
  // https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*)) // _*: spread

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case MyNil => sys.error("no more element")
    case MyCons(_, xs) => xs
  }

  @scala.annotation.tailrec
  def drop[A](list: MyList[A], n: Int): MyList[A] = list match {
    case list if n <= 0 => list  // pattern guard
    case MyNil => MyNil
    case MyCons(_, xs) => drop(xs, n-1)
  }

  @scala.annotation.tailrec
  def dropFromBook[A](l: MyList[A], n: Int): MyList[A] =
    if (n <= 0) l
    else l match {
      case MyNil => MyNil
      case MyCons(_,t) => dropFromBook(t, n-1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](list: MyList[A], f: A => Boolean): MyList[A] = list match {
    case MyCons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => list
  }

  /**
   * 3.3.2 Type information flows from left to right => no need a type annotation for f.
   *
   * This cannot have the same name
   * -> ... have same type after erasure: (list: fpinscala.List, f: Function1)
   */
  @scala.annotation.tailrec
  def dropWhile2[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case MyCons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  def init[A](list: MyList[A]): MyList[A] = list match {
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(x, xs) => MyCons(x, init(xs))
  }

  def setHead[A](list: MyList[A], element: A): MyList[A] = list match {
    case MyNil => MyCons(element, MyNil)
    case MyCons(_, xs) => MyCons(element, xs)
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil => z
      case MyCons(h, t) => f(h, foldRight(t, z)(f))
    }

  def foldRight2[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y, x))

  def foldRight3[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  @scala.annotation.tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    as match {
      case MyNil => z
      case MyCons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def foldRandom[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    as match {
      case MyNil => z
      case MyCons(h, t) => f(foldRandom(t, f(z, h))(f), h)
    }

  def foldLeft2[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((x, y) => f(y, x))

  def sum2(as: MyList[Int]): Int =
    foldRight(as, 0)(_ + _)

  def product2(as: MyList[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0)((_, x) => 1 + x)

  def sum3(as: MyList[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product3(as: MyList[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def length2[A](as: MyList[A]): Int =
    foldLeft(as, 0)((acc, _) => 1 + acc)

  def reverse[A](as: MyList[A]): MyList[A] =
    foldLeft(as, MyNil: MyList[A])((x, y) => MyCons(y, x))

  def append[A](as: MyList[A], x: A): MyList[A] =
    foldRight(as, MyList(x))(MyCons(_, _))

  def append2[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r)(MyCons(_, _))

  def concat[A](as: MyList[MyList[A]]): MyList[A] =
    foldRight(as, MyNil: MyList[A])(append2)

  def add1(as: MyList[Int]): MyList[Int] =
    foldRight(as, MyNil: MyList[Int])((h, t) => MyCons(h+1, t))

  def toString(as: MyList[Double]): MyList[String] =
    foldRight(as, MyNil: MyList[String])((h, t) => MyCons(h.toString, t))

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRight(as, MyNil: MyList[B])((h, t) => MyCons(f(h), t))

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, MyNil:MyList[A])((h, t) => {
      if (f(h)) MyCons(h, t)
      else t
    })

  def flatMap[A](as: MyList[A])(f: A => MyList[A]): MyList[A] =
    foldRight(as, MyNil: MyList[A])((h, t) => append2(f(h), t))

  def flatMap2[A](as: MyList[A])(f: A => MyList[A]): MyList[A] =
    concat(map(as)(f))

  def flatMapFilter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(a => if (f(a)) MyList(a) else MyNil)

  def add0(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    @scala.annotation.tailrec
    def go(l: MyList[Int], r: MyList[Int], acc: MyList[Int]): MyList[Int] = {
      (l, r) match {
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (MyCons(x, xs), MyCons(y, ys)) =>
          go(xs, ys, MyCons(x+y, acc))
      }
    }

    reverse(go(l, r, MyNil)) // -> O(n)
  }

  def add1(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    // n^2 complexity - similar to foldLeft
    @scala.annotation.tailrec
    def go(l: MyList[Int], r: MyList[Int], acc: MyList[Int]): MyList[Int] = {
      (l, r) match {
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (MyCons(x, xs), MyCons(y, ys)) =>
          go(xs, ys, append(acc, x+y))
      }
    }

    go(l, r, MyNil)
  }

  def add2(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    // no tailrec - similar to foldRight
    def go(l: MyList[Int], r: MyList[Int], acc: MyList[Int]): MyList[Int] = {
      (l, r) match {
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (MyCons(x, xs), MyCons(y, ys)) => MyCons(x+y, go(xs, ys, acc))
      }
    }

    go(l, r, MyNil)
  }

  def add3(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
      (l, r) match {
        case (MyNil, _) => MyNil
        case (_, MyNil) => MyNil
        case (MyCons(x, xs), MyCons(y, ys)) => MyCons(x+y, add3(xs, ys))
      }
    }

  def zipWith[A, B](l: MyList[A], r: MyList[A])(f: (A, A) => B): MyList[B] =
    (l, r) match {
      case (MyNil, _) => MyNil
      case (_, MyNil) => MyNil
      case (MyCons(x, xs), MyCons(y, ys)) => MyCons(f(x, y), zipWith(xs, ys)(f))
      case (MyCons(x, xs), MyCons(y, ys)) => MyCons(f(x, y), zipWith(xs, ys)(f))
    }

  def zipWith1[A, B, C](l: MyList[A], r: MyList[B])(f: (A, B) => C): MyList[C] = {
    @scala.annotation.tailrec
    def go(l: MyList[A], r: MyList[B], acc: MyList[C]): MyList[C] = {
      (l, r) match {
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (MyCons(x, xs), MyCons(y, ys)) =>
          go(xs, ys, MyCons(f(x, y), acc))
      }
    }

    reverse(go(l, r, MyNil: MyList[C])) // -> O(n)
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    @scala.annotation.tailrec
    def go[B](l: MyList[B], r: MyList[B]): Boolean = (l, r) match {
      case (_, MyNil) => true
      case (MyNil, _) => false
      case (MyCons(x, xs), MyCons(y, ys)) =>
        if (x == y) go(xs, ys)
        else go(xs, sub)
    }

    go(sup, sub) // -> bug
  }

}

class ch3 extends AnyFunSuite {

  test("Cons") {
    val c = MyCons(1, MyList(2,3))

    assert(c.head == 1)
    assert(c.tail == MyList(2,3))

    assert(MyCons(1, MyNil) == MyList(1))
    assert(MyCons(1, MyCons(2, MyNil)) == MyList(1, 2))
  }

  test("3.1") {
    def matchTest(list: MyList[Int]): Int =
      list match {
        case MyCons(x, MyCons(2, MyCons(4, _))) => x // 1
        case MyNil => 42 // 2
        case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) => x + y // 3
        case MyCons(h, t) => h + MyList.sum(t) // 4
        case _ => 101 // 5
      }

    assert(matchTest(MyList(1,2,4,4,5)) == 1) // 1
    assert(matchTest(MyNil) == 42) // 2
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
    assert(MyList.setHead(MyNil, 3) == MyList(3))
  }

  test("3.4") {
    assert(MyList.drop(MyList(1,2,3), 2) == MyList(3))
    assert(MyList.drop(MyList(1), -1) == MyList(1))
  }

  test("3.5") {
    assert(MyList.dropWhile(MyList(1,2,3), (x: Int) => x < 3) == MyList(3))
    assert(MyList.dropWhile2(MyList(1,2,3))(_ < 3) == MyList(3))
  }

  test("3.6") {
    assert(MyList.init(MyList(1,2,3)) == MyList(1,2))
  }

  test("foldRight") {
    assert(MyList.sum2(MyList(1, 2)) == 3)
    assert(MyList.product2(MyList(2, 3)) == 6)
  }

  test("3.8") {
    // "Nil: List[Int]" is needed to prevent Scala from inferring it as List[Nothing]
    // The last element is Consed first.
    assert(MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(MyCons(_, _)) == MyList(1, 2, 3))

    val a = MyNil
    val b = MyNil: MyList[Nothing]
    val c = MyNil: MyList[Int]

    assert(a == b)
    assert(a == c)
  }

  test("3.9") {
    assert(MyList.length(MyNil) == 0)
    assert(MyList.length(MyList()) == 0)
    assert(MyList.length(MyList(1,2,3)) == 3)
  }

  test("3.10") {
    // The head is added first
    assert(MyList.foldLeft(MyList(1,2,3), 0)(_ + _) == 6)
    assert(MyList.foldLeft(MyList(1,2,3), MyNil: MyList[Int])((x, y) => MyCons(y, x)) == MyList(3, 2, 1))
  }

  test("3.11") {
    assert(MyList.sum3(MyList(1,2,3)) == 6)
    assert(MyList.product3(MyList(1,2,3,4)) == 24)
    assert(MyList.length2(MyList(1,2,3)) ==3)
  }

  test("3.12") {
    assert(MyList.reverse(MyList(1,2,3)) == MyList(3,2,1))
  }

  test("3.13") {
    assert(MyList.foldRight2(MyList(1,2,3), MyNil: MyList[Int])(MyCons(_, _)) == MyList(1,2,3))
    assert(MyList.foldLeft2(MyList(1,2,3), MyNil: MyList[Int])((x, y) => MyCons(y, x)) == MyList(3,2,1))
    assert(MyList.foldRight3(MyList(1,2,3), MyNil: MyList[Int])(MyCons(_, _)) == MyList(1,2,3))
  }

  test("3.14") {
    assert(MyList.append(MyList(1), 2) == MyList(1,2))
    assert(MyList.append2(MyList(1), MyList(2)) == MyList(1,2))
  }

  test("3.15") {
    assert(MyList.concat(MyList(MyList(1),MyList(2))) == MyList(1,2))
  }

  test("3.16") {
    assert(MyList.add1(MyList(1,2,3)) == MyList(2,3,4))
  }

  test("3.17") {
    assert(MyList.toString(MyList(1.0,2.0)) == MyList("1.0", "2.0"))
  }

  test("3.18") {
    assert(MyList.map(MyList(1, 2))(_ * 3) == MyList(3, 6))
  }

  test("3.19") {
    assert(MyList.filter(MyList(1,2,3,4))(_ % 2 == 0) == MyList(2,4))
  }

  test("3.20") {
    assert(MyList.flatMap(MyList(1,2))(x => MyList(x,x)) == MyList(1,1,2,2))
    assert(MyList.flatMap2(MyList(1,2))(x => MyList(x,x)) == MyList(1,1,2,2))
  }

  test("3.21") {
    assert(MyList.flatMapFilter(MyList(1,2,3,4))(_ % 2 == 0) == MyList(2,4))
  }

  test("3.22") {
    assert(MyList.add0(MyList(1,2,3), MyList(1,2,3)) == MyList(2,4,6))
    assert(MyList.add0(MyList(1,2), MyList(1,2,3)) == MyList(2,4))

    assert(MyList.add1(MyList(1,2,3), MyList(1,2,3)) == MyList(2,4,6))
    assert(MyList.add1(MyList(1,2), MyList(1,2,3)) == MyList(2,4))

    assert(MyList.add2(MyList(1,2,3), MyList(1,2,3)) == MyList(2,4,6))
    assert(MyList.add2(MyList(1,2), MyList(1,2,3)) == MyList(2,4))

    assert(MyList.add3(MyList(1,2,3), MyList(1,2,3)) == MyList(2,4,6))
    assert(MyList.add3(MyList(1,2), MyList(1,2,3)) == MyList(2,4))
  }

  test("3.23") {
    assert(MyList.zipWith(MyList(1,2), MyList(1,2))(_ + _) == MyList(2,4))
    assert(MyList.zipWith(MyList(1,2), MyList(1,2))(_ * _) == MyList(1,4))
    assert(MyList.zipWith1(MyList(1,2), MyList(1,2))(_ * _) == MyList(1,4))
    assert(MyList.zipWith1(MyList(1,2,3), MyList(1,2))(_ * _) == MyList(1,4))
  }

  test("3.24") {
    assert(MyList.hasSubsequence(MyList(1,2,3), MyList(1)))
    assert(MyList.hasSubsequence(MyList(1,2,3), MyList(2,3)))
    assert(MyList.hasSubsequence(MyList(1,2,4,3,2,3), MyList(2,3)))

    // This is supposed to be true.
    assert(!MyList.hasSubsequence(MyList(2,2,2,3), MyList(2,2,3)))

    assert(!MyList.hasSubsequence(MyList(2,1,3), MyList(2,3)))
    assert(!MyList.hasSubsequence(MyList(1,2,4,2), MyList(2,3)))
    assert(!MyList.hasSubsequence(MyList(1,2,4,3,2), MyList(2,3)))
  }

  test("foldRandom") {
    assert(MyList.foldRandom(MyList(1,2,3), MyNil: MyList[Int])((a, b) => MyCons(b, a))
      == MyList(1,2,3,3,2,1))
  }
}
