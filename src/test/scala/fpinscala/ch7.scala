package fpinscala

import fpinscala.Par.{Par, parFilter, sequence}
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

object Par {
  // Why is it okay to alias Par when there's a Par object?
  //   Because it doesn't conflict.(?)
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(p: Par[A]): Future[A] = p(s)

  /**
   * Isn't `unit` already lazy since it returns Par[A]?
   *  (Author's answer)
   *  `unit` is represented as a function that returns a `UnitFuture`,
   *  which is a simple implementation of `Future` that just wraps a constant value.
   *  It doesn't use the `ExecutorService` at all.
   *  It's always done and can't be cancelled.
   *  Its `get` method simply returns the value that we gave it.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

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

  // ex 7.4 but the implementation of `fork` is not introduced yet...
  //
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sort(p: Par[List[Int]]): Par[List[Int]] = map(p)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    // Why this doesn't work? (when ch6 sequence does that)
    // ps.foldRight(unit(List[A]))((p, acc) => map2(p, acc)((a, b) => a :: b))
    ps.foldRight[Par[List[A]]](unit(List()))((p, acc) => map2(p, acc)((a, b) => a :: b))
  }

  // Why fork?
  //   To return immediately. (f will be called when es is passed.)
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
    // Why asyncF is needed?
    //   To run it asynchronously.
    val ps: List[Par[B]] = as.map(asyncF(f))
    sequence(ps)
  }

  // from the author
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
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
      es.submit(() => 2)

    val f: (Int, Int) => Int = (a, b) => a + b

    val c = Par.map2(pa, pb)(f)

    // Is ExecutorService a thread pool?
    //   It can manage a pool of threads.
    val es = Executors.newSingleThreadExecutor()

    assert(c(es).get == 3)

    // How to determine the value that is returned when timeout?
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

}
