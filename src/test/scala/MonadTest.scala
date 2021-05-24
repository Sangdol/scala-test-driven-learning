import org.scalatest.funsuite.AnyFunSuite

class MonadTest extends AnyFunSuite {

  /**
    * From Programming Scala, 2nd Edition
    *
    * Monad abstracts `flatMap`.
    * It's named after monas, "the Divinity from which all other things are generated".
    *
    * Monad Laws:
    *  * flatMap(unit(x))(f) == f(x)      Where x is a value
    *  * flatMap(m)(unit) == m            Where m is a Monad instance
    */
  trait Monad[M[_]] {
    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
    def unit[A](a: => A): M[A]

    //
    // Some common aliases:
    //

    // flatMap aka bind
    def bind[A, B](fa: M[A])(f: A => M[B]): M[B] = flatMap(fa)(f)

    // Haskell name
    def >>=[A, B](fa: M[A])(f: A => M[B]): M[B] = flatMap(fa)(f)

    // unit aka pure
    // an abstraction with just unit and pure is called Applicative.
    def pure[A](a: => A): M[A] = unit(a)

    // backticks to avoid keyword (Haskell name)
    def `return`[A](a: => A): M[A] = unit(a)
  }

  // Seq, Option, etc. are monadic since they support flatMap and construction.
  object SeqM extends Monad[Seq] {
    def flatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq flatMap f
    def unit[A](a: => A): Seq[A] = Seq(a)
  }

  object OptionM extends Monad[Option] {
    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] =
      opt flatMap f
    def unit[A](a: => A): Option[A] = Option(a)
  }

  test("test") {
    val seqF: Int => Seq[Int] = i => 1 to i
    val optionF: Int => Option[Int] = i => Some(i + 1)

    assert(SeqM.flatMap(Seq(1, 2))(seqF) == Seq(1, 1, 2))
    assert(OptionM.flatMap(Option(1))(optionF) == Option(2))
  }

  test("laws") {
    val seqF: Int => Seq[Int] = i => 1 to i

    // flatMap(unit(x))(f) == f(x)      Where x is a value
    assert(SeqM.flatMap(SeqM.unit(2))(seqF) == Seq(1, 2))

    // flatMap(m)(unit) == m            Where m is a Monad instance
    assert(SeqM.flatMap(Seq(2))(SeqM.unit(_)) == Seq(2))
  }

  test("composition") {
    import OptionM.flatMap

    val f: Int => Some[Int] = i => Some(i + 1)
    val g: Int => Some[Int] = i => Some(i * 2)
    val m = Some(1)

    val left = flatMap(flatMap(m)(f))(g)
    val right = flatMap(m)(x => flatMap(f(x))(g))

    assert(left.contains(4))
    assert(left == right)
  }

}
