package fpinscala

import org.scalatest.funsuite.AnyFunSuite

// +E, +A: "+" to accommodate Nothing, Exception, etc. (variance)
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(v) => MyLeft(v)
    case MyRight(v) => MyRight(f(v))
  }

  // Why "EE >: E"
  //   It doesn't have any practical reason because the value of Left doesn't change.
  //   -> it actually has a practical reason since we can only flatMap() to
  //      a super type. Need to test a bit more.
  // Then why it's even needed?
  //   Because we need this type signature for f.
  // Then why isn't "B >: A" needed?
  //   Because we're changing the value of A to B using f
  //   on the other hand we're keeping the value of Left as is.
  // Why can't we just use E?
  //   "Covariant type E occurs in contravariant position"
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(v) => MyLeft(v)
    case MyRight(v) => f(v)
  }

  // Why not this (like 5.14)?
//  def flatMap2[E, B](f: A => MyEither[E, B]): MyEither[E, B] = this match {
//    case Left(v) => Left(v)
//    case Right(v) => f(v)
//  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(v) => MyRight(v)
  }

  // We don't need "EE >: E" because "b" is something new.
  def orElse2[C, B >: A](b: => MyEither[C, B]): MyEither[C, B] = this match {
    case MyLeft(_) => b
    case MyRight(v) => MyRight(v)
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = this match {
    case MyLeft(v) => MyLeft(v)
    case MyRight(v) => b.flatMap(bb => MyRight(f(v, bb)))
//    case Right(v) => b.map(bb => f(v, bb))  // this is also possible
  }

  def map22[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = (this, b) match {
    case (_, MyLeft(v)) => MyLeft(v)
    case (MyLeft(v), _) => MyLeft(v)
    case (MyRight(v), MyRight(bb)) => MyRight(f(v, bb))
  }

  def map23[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    b flatMap(bb => this.map(v => f(v, bb)))

  def map24[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      bb <- b
      v <- this
    } yield f(v, bb)
}
// +E so that E can be None (?)
case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]


/**
 * Either Standard Library
 * https://www.scala-lang.org/api/current/scala/util/Either.html
 */
class ch4either extends AnyFunSuite {
  def Try[E, A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e) }

  test("4.6") {
    assert(MyRight(1).map(_ + 2) == MyRight(3))
    assert(MyLeft(1).map(x => x) == MyLeft(1))

    // Why doesn't this work?
    //   because map() is about Right and the type of Right is not defined.
//    assert(Left(1).map(_) == Left(1))
    // No, this doesn't work either.
    val l: MyEither[Int, Int] = MyLeft(1)
    // l.map(_) == "x => l.map(x)"
    // l.map(_ + 1) => l.map(x => x + 1)
    assert(l.map(_ + 0) == MyLeft(1))

    assert(MyRight(2).flatMap(x => MyRight(x + 2)) == MyRight(4))
    assert(MyLeft(2).flatMap(x => MyLeft(x)) == MyLeft(2))
    assert(MyLeft(3).flatMap(x => MyLeft(x.toString)) == MyLeft(3))

    assert(MyLeft(2).orElse(MyRight(1)) == MyRight(1))
    assert(MyLeft(2).orElse2(MyRight(1)) == MyRight(1))
    assert(MyRight(2).orElse(MyRight(1)) == MyRight(2))

    assert(MyLeft(2).map2(MyRight(1))((x, y) => x) == MyLeft(2))
    assert(MyRight(2).map2(MyRight(1))((x, y) => x + y) == MyRight(3))

    val e: MyEither[Int, Int] = MyLeft(2)
    assert(e.map2(MyRight(1))((x, y) => x + y) == MyLeft(2))
  }

  test("4.7") {
    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      es.foldRight[MyEither[E, List[A]]](MyRight(Nil))((e, acc) => e.map2(acc)(_ :: _))

    assert(sequence(List(MyRight(1), MyRight(2))) == MyRight(List(1, 2)))
    assert(sequence(List(MyRight(1), MyRight(2), MyLeft(1))) == MyLeft(1))
    assert(sequence(List(MyLeft(3), MyRight(2), MyLeft(1))) == MyLeft(3))

    def sequence1[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      es.foldRight[MyEither[E, List[A]]](MyRight(Nil))((e, acc) => e.map22(acc)(_ :: _))

    assert(sequence1(List(MyLeft(3), MyRight(2), MyLeft(1))) == MyLeft(1))

    def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
      as.foldRight[MyEither[E, List[B]]](MyRight(Nil))((e, acc) => f(e).map2(acc)(_ :: _))

    assert(traverse(List(1, 2))(x => MyRight(x * 2)) == MyRight(List(2, 4)))

    def sequence2[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      traverse(es)(x => x)
  }
}
