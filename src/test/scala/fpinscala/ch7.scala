package fpinscala

import fpinscala.Par._
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration

object Par {
  // Why is it okay to alias Par when there's a Par object?
  //   Because it doesn't conflict.(?)
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(p: Par[A]): Future[A] = p(s)

  /**
   * Isn't `unit` already lazy since it returns Par[A]?
   *  The argument `a` is strict.
   *
   *  (Author's answer)
   *  `unit` is represented as a function that returns a `UnitFuture`,
   *  which is a simple implementation of `Future` that just wraps a constant value.
   *  It doesn't use the `ExecutorService` at all.
   *  It's always done and can't be cancelled.
   *  Its `get` method simply returns the value that we gave it.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // Why we need `fork` when `unit` is already Par[A]?
  //   To evaluate the argument in a separated thread.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // This takes two threads - one for Callable and another for the inside logic.
  def fork[A](fa: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call: A = fa(es).get
  })

  // Why `get` is a value not a function?
  //   It's a shortened form of a function body.
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = false
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    // What is the use of ExecutorService here?
    //   'es' is passed to Par which has a implementation that uses ExecutorService.
    (es: ExecutorService) => {
      val af = pa(es) // Future[A]
      val bf = pb(es)
      // How is the implementation of 'af: Future' decided?
      //   It's from the implementation of pa.
      //   E.g., AbstractExecutorService#newTaskFor: FutureTask
      UnitFuture(f(af.get, bf.get)) // af.get: A, f: C
    }
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] =
    map2(map2(pa, pb)((a, b) => (c:C) => f(a, b, c)), pc)(_ (_))
//    map2(map2(pa, pb)((a, b) => (c:C) => f(a, b, c)), pc)((e: C => D, c) => e(c))

  def map4[A,B,C,D,E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A,B,C,D) => E): Par[E] =
    map2(map2(map2(pa, pb)((a, b) => (c:C) => (d:D) => f(a, b, c, d)), pc)(_ (_)), pd)(_ (_))

  // seriously?
  def map5[A,B,C,D,E,F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A,B,C,D,E) => F): Par[F] =
    map2(map2(map2(map2(pa, pb)((a, b) => (c:C) => (d:D) => (e:E) => f(a, b, c, d, e)), pc)(_ (_)), pd)(_ (_)), pe)(_ (_))

  // Can I overload map2 function?
  def map2timeout[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C)(timeout: Long, unit: TimeUnit): Par[C] = {
    (es: ExecutorService) => {
      val af = pa(es) // Future[A]
      val bf = pb(es)

      // av and bv are not parallel.
      val duration = Duration(timeout, unit).toMillis
      val afStart = System.currentTimeMillis()
      val av = af.get(duration, TimeUnit.MILLISECONDS)
      val elapsed = System.currentTimeMillis() - afStart
      val remaining = duration - elapsed
      val bv = bf.get(remaining, TimeUnit.MILLISECONDS)

      UnitFuture(f(av, bv))
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(p: Par[List[Int]]): Par[List[Int]] = map(p)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]()))((p, acc) => map2(p, acc)(_ :: _))
    // or
    // ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))
  }

  // Why fork?
  //   To return immediately. (f will be called when es is passed.)
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
    // Why asyncF is needed?
    //   To run it asynchronously.
    val ps: List[Par[B]] = as.map(asyncF(f))
    sequence(ps)
  }

  // From the author - Is this the only answer? looks quite complicated and redundant.
  //   It would have been able to avoid using the list of a list
  //   if it used a sentinel value like null
  //   but Scala doesn't seem to like it.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    // Why do we need `map` here? Can't we just filter?
    //   We need `map` to apply `asyncF`.
    val ps: List[Par[List[A]]] = as.map(
      asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(ps))(_.flatten)
  }

  // This look better than the author's answer - no redundant lists.
  def parFilterOption[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val ps: List[Par[Option[A]]] = as.map(
      asyncF((a: A) => Some(a).filter(f)))

    // Par[List[Option[A]]]
    map(sequence(ps))(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  // monoid
//  type T such there exist:
//    1) zero: T // one?
//    2) f(T, T) => T // f: plus or times
  //
  // string is monoid
  // 1) zero: ""
  // 2) f: concatenation
  def par[T](ints: IndexedSeq[T])(f: (T, T) => T)(default: T): Par[T] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse default)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(par(l)(f)(default), par(r)(f)(default))(f)
    }

  /**
   * @param a  a list of paragraphs
   */
  def wordCount(a: List[String]): Par[Int] =
    if (a.size <= 1) {
      val s = a.headOption getOrElse ""
      unit(s.trim().split(" ").length)
    } else {
      val (l, r) = a.splitAt(a.length / 2)
      map2(wordCount(l), wordCount(r))(_ + _)
    }

  def parString(a: List[String])(f: String => Int)(g: (Int, Int) => Int): Par[Int] =
    if (a.size <= 1) {
      val s = a.headOption getOrElse ""
      unit(f(s))
    } else {
      val (l, r) = a.splitAt(a.length / 2)
      map2(parString(l)(f)(g), parString(r)(f)(g))(g)
    }

  def parStringMoreGeneric[A, B](a: List[A])(f: A => B)(g: (B, B) => B)(default: A): Par[B] =
    if (a.size <= 1) {
      val s = a.headOption getOrElse default
      unit(f(s))
    } else {
      val (l, r) = a.splitAt(a.length / 2)
      map2(parStringMoreGeneric(l)(f)(g)(default),
        parStringMoreGeneric(r)(f)(g)(default))(g)
    }

  // Even this doesn't use the run() method.
  def equal[A](es: ExecutorService)(pa: Par[A], pb: Par[A]): Boolean =
    pa(es).get == pb(es).get
}

class ch7 extends AnyFunSuite {

  /**
   * "Fix the implementation of map2 so that it respects
   * the contract of timeouts on Future."
   *
   * 'timeout' is passed when 'get' is called.
   * What does 'timeouts on Future' mean?
   *   It probably means that it doesn't respect timeouts even if
   *   it has a timeout argument so we need to fix it.
   *   -> No, it depends on the Future implementation and
   *      it should respect timeouts.
   *
   */
  test("7.3") {
    val pa: Par[Int] = (es: ExecutorService) =>
      es.submit(() => {
        Thread.sleep(10)
        1
      })

    val pb: Par[Int] = (es: ExecutorService) =>
      es.submit(() => {
        Thread.sleep(4)
        2
      })

    val f: (Int, Int) => Int = (a, b) => a + b

    val c = Par.map2(pa, pb)(f)

    // Is ExecutorService a thread pool?
    //   It can manage a pool of threads.
    val es = Executors.newSingleThreadExecutor()

    assert(c(es).get == 3)
    assert(Par.run(es)(c).get == 3)

    // How to determine the value that is returned when timeout?
    //   No need to return - Future will throw an exception.
    val d = Par.map2timeout(pa, pb)(f)(5, TimeUnit.MILLISECONDS)
    assertThrows[TimeoutException](Par.run(es)(d).get)

    // TODO - implement it using Map2Future
    // https://www.scala-exercises.org/fp_in_scala/purely_functional_parallelism
  }

  test("7.5") {
    val pa: Par[Int] = lazyUnit(1)
    val pb: Par[Int] = lazyUnit(2)

    // This can be used if we want to run ExecutorService.
//    val pb: Par[Int] = (es: ExecutorService) =>
//      es.submit(() => 2)

    val ps = sequence(List(pa, pb))

    val es = Executors.newSingleThreadExecutor()
    assert(ps(es).get == List(1, 2))
  }

  test("7.6") {
    // At least 2 threads are needed for this. (ex 7.8)
    val es = Executors.newFixedThreadPool(2)

    val pf = parFilter[Int](List(1, 2, 3))(_ % 2 == 0)
    assert(pf(es).get == List(2))

    val pf2 = parFilterOption[Int](List(1, 2, 3))(_ % 2 == 0)
    assert(pf2(es).get == List(2))
  }

  /**
   * Is there a more general version of the parallel summation function
   * we wrote at the beginning of this chapter?
   * Try using it to find the maximum value of an IndexedSeq in parallel.
   */
  test("general") {
    val es = Executors.newSingleThreadExecutor()
    val paMax: Par[Int] = par(IndexedSeq(1, 2, 3))(Math.max)(0)
    assert(paMax(es).get == 3)

    val paMin: Par[Int] = par(IndexedSeq(1, 2, 3))(Math.min)(0)
    assert(paMin(es).get == 1)
  }

  /**
   * Write a function that takes a list of paragraphs (a List[String])
   * and returns the total number of words across all paragraphs, in parallel.
   * Generalize this function as much as possible.
   *
   * How could we set the upper limit of the number of parallel tasks?
   *   ExecutorService should have the number of threads.
   */
  test("word counter") {
    val es = Executors.newSingleThreadExecutor()
    val wcp: Par[Int] = wordCount(List("a b", " c "))
    assert(wcp(es).get == 3)

    // Generalized version - word count
    val wcf: String => Int = _.trim().split(" ").length
    val wcp2: Par[Int] = parString(List("a b", " c "))(wcf)(_ + _)
    assert(wcp2(es).get == 3)

    // sentence length
    val sl: String => Int = _.length
    val slp: Par[Int] = parString(List("a b", " c "))(sl)(_ + _)
    assert(slp(es).get == 6)

    // min sentence size
    val msp: Par[Int] = parString(List("a b", " c"))(sl)(Math.min)
    assert(msp(es).get == 2)

    // min sentence size
    val msp2: Par[Int] = parStringMoreGeneric(List("a b", " c"))(sl)(Math.min)("")
    assert(msp2(es).get == 2)
  }

  test("map3, map4, and map5") {
    val pa: Par[Int] = lazyUnit(1)
    val pb: Par[Int] = lazyUnit(2)
    val pc: Par[Int] = lazyUnit(3)
    val f3: (Int, Int, Int) => Int = (a, b, c) => (a + b) * c

    val es = Executors.newSingleThreadExecutor()
    assert(Par.run(es)(map3(pa, pb, pc)(f3)).get == 9)

    val pd: Par[Int] = lazyUnit(4)
    val f4: (Int, Int, Int, Int) => Int = (a, b, c, d) => (a + b) * (c + d)
    assert(Par.run(es)(map4(pa, pb, pc, pd)(f4)).get == 21)

    val pe: Par[Int] = lazyUnit(5)
    val f5: (Int, Int, Int, Int, Int) => Int = (a, b, c, d, e) => (a + b) * (c + d) + e
    assert(Par.run(es)(map5(pa, pb, pc, pd, pe)(f5)).get == 26)
  }

  /**
   * Given:  map(y)(id) == y
   * then: map(unit(x))(f) == unit(f(x))
   * because "map can't behave differently for different function types it receives"
   * because "it only pass along what it receives"
   *
   * Given:  map(y)(id) == y
   * Prove this: map(map(y)(g))(f) == map(y)(f compose g)
   */
  test("7.7") {
    /**
     * Wrong: circular reasoning
     *
     * Given:  map(y)(id) == y
     * then:  map(y)(id1) == id1(y)
     * then: map(map(y)(id1))(id2) = id2(id1(y)) = map(y)(id2 compose id1)
     * because id(y) == map(y)(id)
     *
     * when id1 = g, id2 = f
     * then: map(map(y)(g))(f) = id2(id1(y)) = map(y)(f compose g)
     */

    /**
     * Answer from the blue book: https://github.com/quchen/articles/blob/master/second_functor_law.md
     *
     *   f .      g =      p .      q         -- (1) Given this ...
     *   => fmap f . fmap g = fmap p . fmap q -- (2) ... this holds
     *
     * how did the second line deduced?
     *
     *   because "map can't behave differently for different function types it receives"
     *   because "it only pass along what it recieves"
     */
  }

  test("7.8") {
    // nested fork?
  }
}
