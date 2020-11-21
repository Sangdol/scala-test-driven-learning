package fpinscala

import org.scalatest.funsuite.AnyFunSuite
import State._

// State is not a state but a state transformer.
// S is a state.
case class State[S,+A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B): State[S,B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](s2: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => s2.map(b => f(a, b)))

  // for-comprehension
  def map2for[B,C](s2: State[S,B])(f: (A,B) => C): State[S,C] =
    for {
      a <- this
      b <- s2
    } yield f(a, b)

}

object State {

  def unit[S,A](a: A): State[S, A] =
    State(s => (a, s))

  // need to set type of unit since foldRight doesn't know about it unlike flatmap
  def sequence[S,A](sts: List[State[S,A]]): State[S, List[A]] = {
    sts.foldRight(unit[S,List[A]](List[A]()))((st, acc) => st.map2(acc)(_ :: _))
  }

}

class ch6state extends AnyFunSuite {

  test("6.10") {
    val inc: State[Int, Int] = State(s => (s, s+1))
    val dec: State[Int, Int] = State(s => (s, s-1))

    assert(inc.run(1) == (1, 2))
    assert(inc.map(n => n * 3).run(1) == (3, 2))

    // (1, 2), (2, 1) => (1+2, 1)
    assert(inc.map2(dec)((a,b) => a + b).run(1) == (3, 1))
    assert(inc.map2for(dec)((a,b) => a + b).run(1) == (3, 1))
    // State[S, List[inc, dec]]
    assert(sequence(List(inc, dec)).run(1) == (List(1, 2), 1))
  }

}
