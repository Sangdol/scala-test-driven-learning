package fpinscala

import org.scalatest.funsuite.AnyFunSuite

object Par {
  def unit[A](a: => A): Par[A]

  def get[A](a: Par[A]): A

  // ex 7.1 - signature
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C]

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}

class ch7 extends AnyFunSuite {

  // sequential
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b)  => a + b)

  // can be parallel
//  def sum(ints: IndexedSeq[Int]): Int =
//    if (ints.size <= 1)
//      ints.headOption getOrElse 0
//    else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      val sumL: Par[Int] = Par.unit(sum(l))
//      val sumR: Par[Int] = Par.unit(sum(r))
//      Par.get(sumL) + Par.get(sumR)
//    }



}
