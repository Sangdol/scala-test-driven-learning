package fpinscala

import org.scalatest.funsuite.AnyFunSuite

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(get) => f(get)
  }

  // "=> B": default won't be evaluated it's needed by the function
  //   (default is a value. why is it needed here?)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def flatMap2[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  // what does ob stand for?
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(get) => Some(get)
  }

  def orElse2[B>:A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse ob

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

class ch4 extends AnyFunSuite {
  test("4.1") {
    // map
    assert(Some(2).map(_ * 3) == Some(6))
    assert(None.map((v: Int)  => v + 2) == None)

    // my flatMap
    assert(Some(2).flatMap((v: Int) => Some(v.toString)) == Some("2"))

    // getOrElse
    assert(Some(2).getOrElse(3) == 2)
    assert(None.getOrElse(3) == 3)

    // flatMap
    assert(Some(2).flatMap2((v: Int) => Some(v.toString)) == Some("2"))

    // orElse
    assert(Some(2).orElse(Some(3)) == Some(2))
    assert(None.orElse(Some(3)) == Some(3))
    assert(Some(2).orElse2(Some(3)) == Some(2))
  }
}
