package fpinscala

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

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
    assert(Some(2).flatMap((v: Int) => Some(v.toString)) == Some("2"))
    assert(None.flatMap((v) => Some(v.toString)) == None)

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

  test("4.2") {
    def variance(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) return None

      val m: Double = xs.sum / xs.size

      // why fold() didn't work?
      val variance = xs.foldLeft(0.0)((acc, a) => acc + math.pow(a-m, 2)) / xs.size

      Some(variance)
    }

    assert(variance(Seq(1, 1, 1)) == Some(0))
    assert(variance(Seq(1, 4, 1)) == Some(2.0))
    assert(variance(Seq()) == None)

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.size)

    def variance2(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

    assert(variance2(Seq(1, 1, 1)) == Some(0))
    assert(variance2(Seq(1, 4, 1)) == Some(2.0))
    assert(variance2(Seq()) == None)
  }

  // Lift can be used to make ordinary functions become Option-compatible functions
  test("Lift") {
    // what is the difference between this and flatMap?
    //   => you can't create a function like abs0 with flatMap.
    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    // why "lift math.abs" can't be understood by the compiler?
    def absO: Option[Double] => Option[Double] = lift(math.abs)

    assert(absO(Some(-1.0)) == Some(1.0))
  }

  test("4.3") {

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
      (a, b) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(get_a), Some(get_b)) => Some(f(get_a, get_b))
      }

    assert(map2(Some(2), Some(3))(_ + _) == Some(5))

    def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
      a flatMap(aa => b map(bb => f(aa, bb)))

    assert(map2_2(Some(2), Some(3))(_ + _) == Some(5))
  }

  // Difficult!
  test("4.4") {
    // MyList vs. List
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      // MyNil vs. Nil
      @tailrec
      def loop(a: List[Option[A]], ol: Option[List[A]]): Option[List[A]] = (a, ol) match {
        case (None :: _, _) => None
        case (Nil, _) => ol
        case (Some(head) :: tail, Some(l)) => loop(tail, Some(l :+ head))
      }

      loop(a, Some(Nil))
    }

    assert(sequence(Nil) == Some(Nil))
    assert(sequence(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(sequence(List(Some(1), None)) == None)

    // None.flatMap() => None
    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
      a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
      }

    assert(sequence2(Nil) == Some(Nil))
    assert(sequence2(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(sequence2(List(Some(1), None)) == None)

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
      a flatMap(aa => b map(bb => f(aa, bb)))

    // map2 returns None if one of the argument is None
    def sequence3[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

    assert(sequence3(Nil) == Some(Nil))
    assert(sequence3(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(sequence3(List(Some(1), None)) == None)
  }
}
