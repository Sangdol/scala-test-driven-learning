package fpinscala

import org.scalatest.funsuite.AnyFunSuite
import MyStream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait MyStream[+A] {

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
    def go(s: MyStream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, Nil).reverse
  }

  def toList3: List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def go(s: MyStream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def take(n: Int): MyStream[A] = {
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
    }
  }

  def take2(n: Int): MyStream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(_, t) =>
      if (n > 1) t().drop(n-1)
      else t()
  }

  def drop2(n: Int): MyStream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop2(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // Why is this foldRight when it evaluates from the left?
  //  It's not about evaluating from the left but about accumulating on the right.
  //  It'll go into the recursion of the right side first when it evaluates.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhile2(p: A => Boolean): MyStream[A] =
    foldRight(Empty: MyStream[A])((h, t) =>  {
//      This evaluates "t" and change the order of execution.
//      (similar to heisenbug)
//      println(h, t)
//
//      This is okay.
//      println(h)
      if (p(h)) cons(h, t) else Empty
    })

  // Is there any use of this?
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Option(h))

  def map[B](f: A => B): MyStream[B] =
    foldRight(Empty: MyStream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): MyStream[A] =
    foldRight(Empty: MyStream[A])((h, t) => if (p(h)) cons(h, t) else t)

  // difficult to find out the spec..
  def append[AA >: A](s: => MyStream[AA]): MyStream[AA] =
    foldRight(s)((h, t) => cons(h,t))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(Empty: MyStream[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def unfoldMap[B](f: A => B): MyStream[B] =
    unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def unfoldTake(n: Int): MyStream[A] =
    unfold((this, n))({
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None
    })

  // From the answer: why does it handle the n=1 case specially?
  def takeViaUnfold(n: Int): MyStream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def unfoldTakeWhile(p: A => Boolean): MyStream[A] =
    unfold(this)({
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    })

  def zipWith[B](s: MyStream[B])(f: (A, B) => B): MyStream[B] =
    unfold((this, s))({
      case (Cons(_, _), Empty) => None
      case (Empty, Cons(_, _)) => None
      case (Cons(lh, lt), Cons(rh, rt)) => Some(f(lh(), rh()), (lt(), rt()))
      case _ => None
    })

  def zipAll[B](s: MyStream[B]): MyStream[(Option[A], Option[B])] =
    unfold((this, s))({
      case (Cons(lh, lt), Empty) => Some((Some(lh()), None), (lt(), Empty))
      case (Empty, Cons(rh, rt)) => Some((None, Some(rh())), (Empty, rt()))
      case (Cons(lh, lt), Cons(rh, rt)) => Some((Some(lh()), Some(rh())), (lt(), rt()))
      case _ => None
    })

  def zipAll2[B](s: MyStream[B]): MyStream[(Option[A], Option[B])] =
    unfold((this, s)){
      case (Empty, Empty) => None
      case (s1, s2) => Some((s1.headOption, s2.headOption), (s1.drop(1), s2.drop(1)))
    }

  // Thinking out of the box - using multiple methods...
  def startsWith[A](s: MyStream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll({
      case (l, r) => l == r
    })

  def startsWithGrisha[A](that: MyStream[A]): Boolean = (this, that) match {
    case (_, Empty)                         => true
    case (Cons(a,b), Cons(x,y)) if a()==x() => b().startsWithGrisha(y())
    case _                                  => false
  }

  def startsWithDuy[A](s: MyStream[A]): Boolean = zipAll(s).foldRight(true){
    case ((Some(a), Some(b)), flag) => a == b && flag
    case ((None, Some(_)), _) => false
    case ((Some(_), None), flag) => flag
  }

  def tails: MyStream[MyStream[A]] =
    unfold(this){
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    }

  def hasSubsequence[A](s: MyStream[A]): Boolean =
    tails exists (_ startsWith s)

  // Wrong
  def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] =
    unfold(tails)({
      case Cons(h, t) => Some(h().foldRight(z)(f), t())
      case _ => None
    }).append(MyStream(z))

  // difficult
  def scanRight2[B](z: B)(f: (A, => B) => B): MyStream[B] = {
    // Why is it okay to pass (z, (Stream(z)) which isn't B
    // as the first argument of foldRight?
    //   B of foldRight is (B, Stream[B]) of scanRight2.
    foldRight((z, MyStream(z)))((h, t) => {
      lazy val t1 = t
      val b2 = f(h, t1._1)
      (b2, cons(b2, t1._2))
    })._2
  }

  // Does it need to be lazy?
  def scanRightNonLazy[B](z: B)(f: (A, => B) => B): MyStream[B] = {
    // Why is it okay to pass (z, (Stream(z)) which isn't B
    // as the first argument of foldRight?
    //   B of foldRight is (B, Stream[B]) of scanRight2.
    foldRight(MyStream(z))((h, t) => {
      val b2 = f(h, t.headOption.get)
      cons(b2, t)
    })
  }
}

case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

  val ones: MyStream[Int] = MyStream.cons(1, ones)

  def constant[A](a: A): MyStream[A] =
    MyStream.cons(a, constant(a))

  def from(n: Int): MyStream[Int] =
    MyStream.cons(n, from(n+1))

  def fibs: MyStream[Int] = {
    def go(c: Int, n: Int): MyStream[Int] =
      MyStream.cons(c, go(n, c+n))

    go(0, 1)
  }

  /**
   * Corecursive function:
   *   - produces data <-> recursive function consumes data
   *   - aka guarded recursion - f guards by returning None (Option)
   *
  * Why Option is used? to stop when f returns None
   */
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): MyStream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  val ones2: MyStream[Int] = unfold(1)(x => Option((x, x)))

  def constant2[A](a: A): MyStream[A] = unfold(a)(x => Option((x, x)))

  def from2(n: Int): MyStream[Int] = unfold(n)(x => Option((x, x+1)))

  // case is a way to unpack a tuple (it's a partial function)
  // or you could do t._1, t._2
  val fibs2: MyStream[Int] = unfold((0,1))({ case (c,n) => Some((c, (n, c+n))) })

  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val h = hd
    lazy val t = tl

    Cons(() => h, () => t)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
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

    def if4[A](cond: Boolean, onTrue: A, onFalse: A): A =
      if (cond) onTrue else onFalse

    assert(1 == if3(cond = true, 1, 2))
    assert(1 == if3(cond = true, 1, {
      print("this doesn't print")
      2
    }))
    assert(1 == if4(cond = true, 1, {
      print("this prints")
      2
    }))

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
    assert(counter2 == 1)  // no double evaluation
  }

  test("cons") {
    assert(List(1) == cons(1, Empty).toList)

  }

  test("5.1") {
    assert(Option(1) == MyStream(1).headOption)
    assert(MyStream().headOption.isEmpty)
    assert(List(1,2,3) == MyStream(1,2,3).toList)

    assert(List(1,2,3) == MyStream(1,2,3).toList2)
    assert(List(1,2,3) == MyStream(1,2,3).toList3)
  }

  test("5.2") {
    assert(List(1,2) == MyStream(1,2,3).take(2).toList)
    assert(List(1,2) == MyStream(1,2,3).take2(2).toList)

    // drop
    assert(List() == MyStream().drop(1).toList)
    assert(List() == MyStream(1).drop(1).toList)
    assert(List(2) == MyStream(1,2).drop(1).toList)
    assert(List(3) == MyStream(1,2,3).drop(2).toList)

    assert(List() == MyStream().drop2(1).toList)
    assert(List() == MyStream(1).drop2(1).toList)
    assert(List(2) == MyStream(1,2).drop2(1).toList)
    assert(List(3) == MyStream(1,2,3).drop2(2).toList)
  }

  test("5.3") {
    assert(List() == MyStream(1,2).takeWhile(_ > 2).toList)
    assert(List() == MyStream(1,2,3,4).takeWhile(_ > 2).toList)
    assert(List(3,4) == MyStream(3,4,1,2,3,4).takeWhile(_ > 2).toList)
  }

  test("exist") {
    assert(MyStream(1,2).exists(_ == 1))
    assert(!MyStream(1,2).exists(_ == 3))

    assert(MyStream(1,2).exists2(_ == 1))
    assert(!MyStream(1,2).exists2(_ == 3))
  }

  test("5.4") {
    assert(MyStream(1,2).forAll(_ > 0))
    assert(!MyStream(0,1,2,3).forAll(_ > 2))
  }

  test("5.5") {
    // ducking difficult to understand
    assert(List() == MyStream(1,2).takeWhile2(_ > 2).toList)
    assert(List() == MyStream(1,2,3,4).takeWhile2(_ > 2).toList)
    assert(List(3,4) == MyStream(3,4,1,2,3,4).takeWhile2(_ > 2).toList)
    assert(List(4,3) == MyStream(4,3,2,1).takeWhile2(_ > 2).toList)
  }

  test("5.6") {
    assert(Option(1) == MyStream(1).headOption2)
    assert(MyStream().headOption2.isEmpty)
  }

  test("5.7") {
    assert(List(2,4) == MyStream(1,2).map(_ * 2).toList)
    assert(List(2) == MyStream(1,2,3).filter(_ % 2 == 0).toList)
    assert(List(1,2) == MyStream(1).append(MyStream(2)).toList)
    assert(List(2,4) == MyStream(1,2).flatMap(s => MyStream(s*2)).toList)
  }

  test("infinite") {
    // "forward reference extends over definition of value ones"
    // if `ones` was defined here.
    assert(List(1,1,1) == MyStream.ones.take(3).toList)
  }

  test("5.8") {
    assert(List(2,2,2) == MyStream.constant(2).take(3).toList)
    assert(List("2") == MyStream.constant("2").take(1).toList)
  }

  test("5.9") {
    assert(List(2,3,4) == MyStream.from(2).take(3).toList)
  }

  test("5.10") {
    assert(List(0,1,1,2,3) == MyStream.fibs.take(5).toList)
  }

  test("5.11") {
    assert(List(1,1,1,1) == MyStream.unfold(0)(x => Option((x+1, x))).take(4).toList)
    assert(List(0,1,2,3) == MyStream.unfold(0)(x => Option((x, x+1))).take(4).toList)
  }

  /**
   * About the footnote.
   *
   * The recursive definition consumes constant memory
   * even if we keep a reference to it around while traversing it,
   * while the unfold-based implementation does not.
   *
   * "even if we keep a reference to it around while traversing it"
   * -> what does this mean?
   *    it consumes constant memory because it keeps a reference to it
   *    not "even if".
   *
   * "while the unfold-based implementation does not."
   * -> what does this mean?
   *    does it mean it does not consume constant memory and consume linear memory?
   *    why? it should also keep a referent to the value.
   *
   *    or does it mean that it doesn't keep a reference?
   *    it makes more sense.
   *
   * Preserving sharing isn’t something we usually rely on when programming
   * with streams, since it’s extremely delicate and not tracked by the types.
   * For instance, sharing is destroyed when calling even xs.map(x => x).
   *
   * - why it's destroyed? (what does it mean?
   *   - I guess it means that it loses the connection to the sharing.
   *     but why is it a problem?
   *
   *  There's a question but it doesn't have explanation about my questions.
   *  https://stackoverflow.com/questions/55919618/what-does-preserve-sharing-means-in-lazy-streams
   */
  test("5.12") {
    ones.map(x => x)
    assert(List(1,1) == MyStream.ones2.take(2).toList)
    assert(List(2,2) == MyStream.constant2(2).take(2).toList)
    assert(List(2,3) == MyStream.from2(2).take(2).toList)
    assert(List(0,1,1,2,3) == MyStream.fibs2.take(5).toList)
  }

  /**
   * What is the main difference between foldRight and unfold?
   */
  test("5.13") {
    assert(List(2,4) == MyStream(1,2).unfoldMap(_ * 2).toList)

    assert(List(1) == MyStream(1,2).unfoldTake(1).toList)
    assert(List() == MyStream().unfoldTake(1).toList)

    assert(List() == MyStream(1,2).unfoldTakeWhile(_ > 2).toList)
    assert(List() == MyStream(1,2,3,4).unfoldTakeWhile(_ > 2).toList)
    assert(List(3,4) == MyStream(3,4,1,2,3,4).unfoldTakeWhile(_ > 2).toList)

    assert(List(2,4,6) == MyStream(1,2,3).zipWith(MyStream(1,2,3))(_ + _).toList)
    assert(List(2,4,6) == MyStream(1,2,3,4).zipWith(MyStream(1,2,3))(_ + _).toList)

    assert(List((Some(1), None)) == MyStream(1).zipAll(Empty).toList)
    assert(List((Some(1), Some(2))) == MyStream(1).zipAll(MyStream(2)).toList)
    assert(Nil == MyStream().zipAll(MyStream()).toList)

    assert(List((Some(1), None)) == MyStream(1).zipAll2(Empty).toList)
    assert(List((Some(1), Some(2))) == MyStream(1).zipAll2(MyStream(2)).toList)
    assert(Nil == MyStream().zipAll2(MyStream()).toList)
  }

  test("hasSubsequence") {
    assert(MyStream(1,2,3).hasSubsequence(MyStream(2,3)))
  }

  test("5.14") {
    assert(MyStream(1,2,3).startsWith(Empty))
    assert(MyStream(1,2,3).startsWith(MyStream(1)))
    assert(MyStream(1,2,3).startsWith(MyStream(1,2)))
    assert(!MyStream(1,2,3).startsWith(MyStream(1,3)))
    assert(!MyStream(1,2,3).startsWith(MyStream(1,2,4)))

    assert(MyStream(1,2,3).startsWithGrisha(Empty))
    assert(MyStream(1,2,3).startsWithGrisha(MyStream(1)))
    assert(MyStream(1,2,3).startsWithGrisha(MyStream(1,2)))
    assert(!MyStream(1,2,3).startsWithGrisha(MyStream(1,3)))
    assert(!MyStream(1,2,3).startsWithGrisha(MyStream(1,2,4)))

    assert(MyStream(1,2,3).startsWithDuy(Empty))
    assert(MyStream(1,2,3).startsWithDuy(MyStream(1)))
    assert(MyStream(1,2,3).startsWithDuy(MyStream(1,2)))
    assert(!MyStream(1,2,3).startsWithDuy(MyStream(1,3)))
    assert(!MyStream(1,2,3).startsWithDuy(MyStream(1,2,4)))
    assert(!MyStream(1,2,3).startsWithDuy(MyStream(2,2,3)))
    // (1,2) (2,3) (3,None)
    assert(!MyStream(1,2,3).startsWithDuy(MyStream(2,3)))
    assert(!MyStream(1,2,3).startsWithDuy(MyStream(1,2,3,4)))
  }

  test("5.15") {
    assert(List(List(1,2), List(2)) == MyStream(1,2).tails.toList.map(_.toList))
  }

  test("5.16 scanRight") {
    // Can it be implemented using unfold? How, or why not?
    //   "The function can't be implemented using `unfold`,
    //    since `unfold` generates elements of the `Stream`
    //    from left to right.
    //    It can be implemented using `foldRight` though.
    //  -> what couldn't the unfold solution do?
    //
    // Could it be implemented using another function we’ve written?
    //   Probably.
    assert(List(3,2,0) == MyStream(1,2).scanRight(0)(_ + _).toList)
    assert(List(0,0,0) == MyStream(1,2).scanRight(0)(_ * _).toList)
    assert(List(List(1,2), List(2), List()) ==
      MyStream(1,2).scanRight(Empty: MyStream[Int])(cons(_, _)).toList.map(_.toList))

    // This is the correct function.
    // (0, (0)), (2, (2,0)), (3, (3,2,0))
    assert(List(3,2,0) == MyStream(1,2).scanRight2(0)(_ + _).toList)
    assert(List(0,0,0) == MyStream(1,2).scanRight2(0)(_ * _).toList)
    assert(List(List(1,2), List(2), List()) ==
      MyStream(1,2).scanRight2(Empty: MyStream[Int])(cons(_, _)).toList.map(_.toList))

    assert(List(3,2,0) == MyStream(1,2).scanRightNonLazy(0)(_ + _).toList)
    assert(List(0,0,0) == MyStream(1,2).scanRightNonLazy(0)(_ * _).toList)
    assert(List(List(1,2), List(2), List()) ==
      MyStream(1,2).scanRightNonLazy(Empty: MyStream[Int])(cons(_, _)).toList.map(_.toList))
  }
}
