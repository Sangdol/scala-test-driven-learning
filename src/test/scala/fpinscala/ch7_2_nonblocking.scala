package fpinscala

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{
  Callable,
  CountDownLatch,
  ExecutorService,
  Executors
}
import java.util.concurrent.atomic.AtomicReference
import fpinscala.NonblockingPar._

object NonblockingPar {
  sealed trait Future[+A] {
    private[fpinscala] def apply(k: A => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call: Unit = r })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork {
      val ps: List[Par[B]] = as.map(asyncF(f))
      sequence(ps)
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((p, acc) => map2(p, acc)(_ :: _))
}

class ch7_2 extends AnyFunSuite {

  test("run") {
    val p = parMap(List(1, 4))(math.sqrt(_))
    val x = NonblockingPar.run(Executors.newFixedThreadPool(2))(p)

    assert(x == List(1, 2))
  }

  test("7.10") {
    val p: Par[Int] = unit(10)
    val x = NonblockingPar.run(Executors.newFixedThreadPool(2))(p)

    assert(x == 10)

    // It's not swallowed?
//    val p2: Par[Int] = (es: ExecutorService) => new Future[Int] {
//      override private[fpinscala] def apply(k: Int => Unit): Unit = {
//        throw new Exception()
//      }
//    }
//    val x2 = NonblockingPar.run(Executors.newFixedThreadPool(2))(p2)
//
//    assert(x2 == 10)
  }

}
