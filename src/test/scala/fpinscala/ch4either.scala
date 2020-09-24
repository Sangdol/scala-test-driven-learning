package fpinscala

import org.scalatest.funsuite.AnyFunSuite

// +E, +A: "+" to accommodate Nothing, Exception, etc. (variance)
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
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
  // Why can't we just use E?
  //   "Covariant type E occurs in contravariant position"
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  // We don't need "EE >: E" because "b" is something new.
  def orElse2[C, B >: A](b: => MyEither[C, B]): MyEither[C, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = this match {
    case Left(v) => Left(v)
    case Right(v) => b.flatMap(bb => Right(f(v, bb)))
//    case Right(v) => b.map(bb => f(v, bb))  // this is also possible
  }

  def map22[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = (this, b) match {
    case (_, Left(v)) => Left(v)
    case (Left(v), _) => Left(v)
    case (Right(v), Right(bb)) => Right(f(v, bb))
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
case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]


/**
 * Either Standard Library
 * https://www.scala-lang.org/api/current/scala/util/Either.html
 */
class ch4either extends AnyFunSuite {
  def Try[E, A](a: => A): MyEither[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  test("4.6") {
    assert(Right(1).map(_ + 2) == Right(3))
    assert(Left(1).map(x => x) == Left(1))

    // Why doesn't this work?
    //   because map() is about Right and the type of Right is not defined.
//    assert(Left(1).map(_) == Left(1))
    // No, this doesn't work either.
    val l: MyEither[Int, Int] = Left(1)
    // l.map(_) == "x => l.map(x)"
    // l.map(_ + 1) => l.map(x => x + 1)
    assert(l.map(_ + 0) == Left(1))

    assert(Right(2).flatMap(x => Right(x + 2)) == Right(4))
    assert(Left(2).flatMap(x => Left(x)) == Left(2))
    assert(Left(3).flatMap(x => Left(x.toString)) == Left(3))

    assert(Left(2).orElse(Right(1)) == Right(1))
    assert(Left(2).orElse2(Right(1)) == Right(1))
    assert(Right(2).orElse(Right(1)) == Right(2))

    assert(Left(2).map2(Right(1))((x, y) => x) == Left(2))
    assert(Right(2).map2(Right(1))((x, y) => x + y) == Right(3))

    val e: MyEither[Int, Int] = Left(2)
    assert(e.map2(Right(1))((x, y) => x + y) == Left(2))
  }

  test("4.7") {
    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      es.foldRight[MyEither[E, List[A]]](Right(Nil))((e, acc) => e.map2(acc)(_ :: _))

    assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
    assert(sequence(List(Right(1), Right(2), Left(1))) == Left(1))
    assert(sequence(List(Left(3), Right(2), Left(1))) == Left(3))

    def sequence1[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      es.foldRight[MyEither[E, List[A]]](Right(Nil))((e, acc) => e.map22(acc)(_ :: _))

    assert(sequence1(List(Left(3), Right(2), Left(1))) == Left(1))

    def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
      as.foldRight[MyEither[E, List[B]]](Right(Nil))((e, acc) => f(e).map2(acc)(_ :: _))

    assert(traverse(List(1, 2))(x => Right(x * 2)) == Right(List(2, 4)))

    def sequence2[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
      traverse(es)(x => x)
  }
}
