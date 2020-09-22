package fpinscala

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(get) => MySome(f(get))
  }

  // why is it called "flatMap"?
  //  list.flatMap = map to list + append
  //  option.flatMap = map to option (no append)
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(get) => f(get)
  }

  // "=> B": default won't be evaluated it's needed by the function
  //   "The default: => B type annotation in getOrElse (and the similar annotation
  //    in orElse) indicates that the argument is of type B, but won't be evaluated
  //    until it's needed by the function. Don't worry about this for now -
  //    we'll talk much more about this concept of non-strictness in the next chapter."
  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(get) => get
  }

  def flatMap2[B](f: A => MyOption[B]): MyOption[B] =
    map(f).getOrElse(MyNone)

  // what does ob stand for?
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case MySome(get) => MySome(get)
  }

  def orElse2[B>:A](ob: => MyOption[B]): MyOption[B] =
    map (MySome(_)) getOrElse ob

}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

/**
 * Option Standard Library
 * https://www.scala-lang.org/api/current/scala/Option.html
 * (sequence, traverse, and map2 are missing)
 */
class ch4option extends AnyFunSuite {

  test("4.1") {
    // map
    assert(MySome(2).map(_ * 3) == MySome(6))
    assert(MyNone.map((v: Int)  => v + 2) == MyNone)

    // my flatMap
    assert(MySome(2).flatMap((v: Int) => MySome(v.toString)) == MySome("2"))
    assert(MySome(2).flatMap((v: Int) => MySome(v.toString)) == MySome("2"))
    assert(MyNone.flatMap((v) => MySome(v.toString)) == MyNone)

    // getOrElse
    assert(MySome(2).getOrElse(3) == 2)
    assert(MyNone.getOrElse(3) == 3)

    // flatMap
    assert(MySome(2).flatMap2((v: Int) => MySome(v.toString)) == MySome("2"))

    // orElse
    assert(MySome(2).orElse(MySome(3)) == MySome(2))
    assert(MyNone.orElse(MySome(3)) == MySome(3))
    assert(MySome(2).orElse2(MySome(3)) == MySome(2))
  }

  test("4.2") {
    def variance(xs: Seq[Double]): MyOption[Double] = {
      if (xs.isEmpty) return MyNone

      val m: Double = xs.sum / xs.size

      // why fold() didn't work?
      val variance = xs.foldLeft(0.0)((acc, a) => acc + math.pow(a-m, 2)) / xs.size

      MySome(variance)
    }

    assert(variance(Seq(1, 1, 1)) == MySome(0))
    assert(variance(Seq(1, 4, 1)) == MySome(2.0))
    assert(variance(Seq()) == MyNone)

    def mean(xs: Seq[Double]): MyOption[Double] =
      if (xs.isEmpty) MyNone
      else MySome(xs.sum / xs.size)

    def variance2(xs: Seq[Double]): MyOption[Double] =
      mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

    assert(variance2(Seq(1, 1, 1)) == MySome(0))
    assert(variance2(Seq(1, 4, 1)) == MySome(2.0))
    assert(variance2(Seq()) == MyNone)
  }

  // Lift can be used to make ordinary functions become Option-compatible functions
  test("Lift") {
    // what is the difference between this and flatMap?
    //   => you can't create a function like abs0 with flatMap.
    def lift[A,B](f: A => B): MyOption[A] => MyOption[B] = _ map f

    // why "lift math.abs" can't be understood by the compiler?
    def absO: MyOption[Double] => MyOption[Double] = lift(math.abs)

    assert(absO(MySome(-1.0)) == MySome(1.0))
  }

  test("4.3") {

    def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] =
      (a, b) match {
        case (MyNone, _) => MyNone
        case (_, MyNone) => MyNone
        case (MySome(get_a), MySome(get_b)) => MySome(f(get_a, get_b))
      }

    assert(map2(MySome(2), MySome(3))(_ + _) == MySome(5))

    def map2_2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] =
      a flatMap(aa => b map(bb => f(aa, bb)))

    assert(map2_2(MySome(2), MySome(3))(_ + _) == MySome(5))
  }

  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] =
    a flatMap(aa => b map(bb => f(aa, bb)))

  // Difficult!
  test("4.4") {
    // MyList vs. List
    def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
      // MyNil vs. Nil
      @tailrec
      def loop(a: List[MyOption[A]], ol: MyOption[List[A]]): MyOption[List[A]] = (a, ol) match {
        case (MyNone :: _, _) => MyNone
        case (Nil, _) => ol
        case (MySome(head) :: tail, MySome(l)) => loop(tail, MySome(l :+ head))
      }

      loop(a, MySome(Nil))
    }

    assert(sequence(Nil) == MySome(Nil))
    assert(sequence(List(MySome(1), MySome(2))) == MySome(List(1,2)))
    assert(sequence(List(MySome(1), MyNone)) == MyNone)

    // None.flatMap() => None
    def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] =
      a match {
        case Nil => MySome(Nil)
          // Can I use map instead of flatMap here?
          //   No, because map() requires a function that returns a value
          //   but there's no map() that can be used as f
          //   which returns a value.
          // Why can't we make one?
          //   We need a function that returns Option to handle a None element.
        case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
      }

    assert(sequence2(Nil) == MySome(Nil))
    assert(sequence2(List(MySome(1), MySome(2))) == MySome(List(1,2)))
    assert(sequence2(List(MySome(1), MyNone)) == MyNone)

    // map2 returns None if one of the argument is None
    def sequence3[A](a: List[MyOption[A]]): MyOption[List[A]] =
      a.foldRight[MyOption[List[A]]](MySome(Nil))((x, y) => map2(x,y)(_ :: _))

    assert(sequence3(Nil) == MySome(Nil))
    assert(sequence3(List(MySome(1), MySome(2))) == MySome(List(1,2)))
    assert(sequence3(List(MySome(1), MyNone)) == MyNone)
  }

  def Try[A](a: => A): MyOption[A] = {
    try MySome(a)
    catch { case e: Exception => MyNone }
  }

  test("4.5") {
    def traverse[A,B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
      a match {
        case Nil => MySome(Nil)
        case h :: t => f(h) flatMap (hh => traverse(t)(f).map(hh :: _))
      }

    assert(traverse(Nil)(MySome(_)) == MySome(Nil))
    assert(traverse(List("1", "2"))(MySome(_)) == MySome(List("1", "2")))
    assert(traverse(List("1", "2"))(x => Try(x.toInt)) == MySome(List(1, 2)))
    assert(traverse(List("1", "x"))(x => Try(x.toInt)) == MyNone)

    // why this doesn't work? "Cannot resolve symbol toInt"
//    assert(traverse(List("1", "2"))(Try(_.toInt)) == Some(List("1", "2")))

    def traverse2[A,B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
      a match {
        case Nil => MySome(Nil)
        case h :: t => map2(f(h), traverse2(t)(f))(_ :: _)
      }

    assert(traverse2(Nil)(MySome(_)) == MySome(Nil))
    assert(traverse2(List("1", "2"))(MySome(_)) == MySome(List("1", "2")))
    assert(traverse2(List("1", "2"))(x => Try(x.toInt)) == MySome(List(1, 2)))
    assert(traverse2(List("1", "x"))(x => Try(x.toInt)) == MyNone)

    def traverse3[A,B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
      a.foldRight[MyOption[List[B]]](MySome(Nil))((h, t) => map2(f(h), t)(_ :: _))

    assert(traverse3(Nil)(MySome(_)) == MySome(Nil))
    assert(traverse3(List("1", "3"))(MySome(_)) == MySome(List("1", "3")))
    assert(traverse3(List("1", "3"))(x => Try(x.toInt)) == MySome(List(1, 3)))
    assert(traverse3(List("1", "x"))(x => Try(x.toInt)) == MyNone)
  }

  test("for-comprehension") {
    def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)

    assert(map2(MySome(2), MySome(3))(_ + _) == MySome(5))
  }
}

