package fpinscala

import fpinscala.Par._
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit, TimeoutException}
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

  // Is it a correct implementation?
  def fork[A](fa: => Par[A]): Par[A] = es => fa(es)

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

  // ex 7.4 but the implementation of `fork` is not introduced yet...
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sort(p: Par[List[Int]]): Par[List[Int]] = map(p)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    // Why this doesn't work? (when ch6 sequence does that)
    // ps.foldRight(unit(List[A]))((p, acc) => map2(p, acc)(_ :: _))
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))
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

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  def par(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(par(l)(f), par(r)(f))(f)
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
        Thread.sleep(3)
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
  }

  test("7.5") {
    val pa: Par[Int] = (es: ExecutorService) =>
      es.submit(() => 1)

    val pb: Par[Int] = (es: ExecutorService) =>
      es.submit(() => 2)

    val ps = sequence(List(pa, pb))

    val es = Executors.newSingleThreadExecutor()
    assert(ps(es).get == List(1, 2))
  }

  test("7.6") {
    val pa: Par[Int] = (es: ExecutorService) =>
      es.submit(() => 1)

    val pb: Par[Int] = (es: ExecutorService) =>
      es.submit(() => 2)

    val pf = parFilter[Int](List(1, 2))(_ % 2 == 0)

    val es = Executors.newSingleThreadExecutor()
    assert(pf(es).get == List(2))
  }

  /**
   * Is there a more general version of the parallel summation function
   * we wrote at the beginning of this chapter?
   * Try using it to find the maximum value of an IndexedSeq in parallel.
   */
  test("general") {
    val es = Executors.newSingleThreadExecutor()
    val paMax: Par[Int] = par(IndexedSeq(1, 2, 3))(Math.max)
    assert(paMax(es).get == 3)

    val paMin: Par[Int] = par(IndexedSeq(1, 2, 3))(Math.min)
    assert(paMin(es).get == 1)
  }

  /**
   * Write a function that takes a list of paragraphs (a List[String])
   * and returns the total number of words across all paragraphs, in parallel.
   * Generalize this function as much as possible.
   *
   * How could we set the upper limit of the number of parallel tasks?
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
  }
}
