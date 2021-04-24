package fpinscala

import org.scalatest.funsuite.AnyFunSuite
import State._

// State is not a state but a state transformer.
// S is a state.
// Then what is the use of A?
//   A can be also a part of a state,
//   and it can be a value that we're interested in.
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

  // How does `yield ()` return State[S, Unit]?
  //   That's how flatMat+map and for-comprehension work.
  //   Look at the implementation of State.map2() and State.map2for().
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S,S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

class ch6state extends AnyFunSuite {

  test("6.10") {
    val inc: State[Int, Int] = State(s => (s, s + 1))
    val dec: State[Int, Int] = State(s => (s, s - 1))

    assert(inc.run(1) == (1, 2))
    assert(inc.map(n => n * 3).run(1) == (3, 2))

    // (1, 2), (2, 1) => (1+2, 1)
    assert(inc.map2(dec)((a, b) => a + b).run(1) == (3, 1))
    assert(inc.map2for(dec)((a, b) => a + b).run(1) == (3, 1))
    // State[S, List[inc, dec]]
    assert(sequence(List(inc, dec)).run(1) == (List(1, 2), 1))
  }

  // Very difficult
  test("6.11") {
    // simple candy dispenser
    // input: insert coin / turn knob
    // state: locked / unlocked
    // track: how many candies are left / how many coins
    // rules
    //   - coin -> unlock when candy > 0
    //   - turn -> dispense candy -> lock
    //   - locked + turn -> not working
    //   - unlocked + coin -> not working
    //   - no_candy -> ignores all input
    //
    // Why two Inputs? because coin + turn

    object Candy {
      def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
        _ <- sequence(inputs map (modify[Machine] _ compose update))
        s <- get
      } yield (s.coins, s.candies)

      // What is the value of _? (Int, Int)
      // Why do we need (Int, Int) here? it's the end result.
      //   Can't I just take values from Machine or maybe we don't need machine.
      //   The candy and coin values look redundant.
      //   => But there's no other option since the value A is dependent on S.
      //      There's no way to initialize the A value with arbitrary values.
      // Why there's no explicit 'run'? because it's hidden in flatMap.
      def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
        sequence(inputs map (modify[Machine] _ compose update)).flatMap(
          _ => get.map(s => (s.coins, s.candies)))
    }

    // Coin
    // working
    assert(Candy.simulateMachine(List(Coin)).run(Machine(true, 10, 5))._2 == Machine(false, 10, 6))

    // no candy
    assert(Candy.simulateMachine(List(Coin)).run(Machine(true, 0, 5))._2 == Machine(true, 0, 5))
    // already unlocked
    assert(Candy.simulateMachine(List(Coin)).run(Machine(false, 10, 5))._2 == Machine(false, 10, 5))

    // Turn
    // working
    assert(Candy.simulateMachine(List(Turn)).run(Machine(false, 10, 5))._2 == Machine(true, 9, 5))
    // locked
    assert(Candy.simulateMachine(List(Turn)).run(Machine(true, 10, 5))._2 == Machine(true, 10, 5))

    // Multiple steps
    assert(Candy.simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 5))._2 == Machine(true, 9, 6))

    // No candies
    assert(Candy.simulateMachine(List(Coin, Turn)).run(Machine(true, 0, 5))._2 == Machine(true, 0, 5))
  }
}