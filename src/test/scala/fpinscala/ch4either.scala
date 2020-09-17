package fpinscala

import org.scalatest.funsuite.AnyFunSuite

// +E, +A: "+" to accommodate Nothing, Exception, etc. (variance)
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  // Why "EE >: E"
  //   It doesn't have any practical reason because the value of Left doesn't change.
  // Then why it's even needed?
  //   Because we need this type signature for f.
  // Then why isn't "B >: A" needed?
  //   Because we're changing the value of A to B using f
  //   on the other hand we're keeping the value of Left as is.
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

}
// +E so that E can be None (?)
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


/**
 * Either Standard Library
 * https://www.scala-lang.org/api/current/scala/util/Either.html
 */
class ch4either extends AnyFunSuite {
  def Try[E, A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  test("4.6") {
    assert(Right(1).map(_ + 2) == Right(3))
    assert(Left(1).map(x => x) == Left(1))

    // why doesn't this work?
//    assert(Left(1).map(_) == Left(1))

    assert(Right(2).flatMap(x => Right(x + 2)) == Right(4))
    assert(Left(2).flatMap(x => Left(x)) == Left(2))
    assert(Left(3).flatMap(x => Left(x.toString)) == Left(3))
  }
}
