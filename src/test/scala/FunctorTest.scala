import org.scalatest.funsuite.AnyFunSuite

class FunctorTest extends AnyFunSuite {

  /**
    * From Programming Scala, 2nd Edition
    *
    * Functor abstracts the `map` operation.
    */
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object SeqF extends Functor[Seq] {
    def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq map f
  }

  object OptionF extends Functor[Option] {
    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt map f
  }

  object FunctionF {
    def map[A, A2, B](func: A => A2)(f: A2 => B): A => B = {
      // type definition: { type lambda[beta] = A => beta }
      // type lambda: ({ type lambda[beta] = A => beta })#lambda
      // type projection: #
      // "The 'lambda' is an alias for Map with an embedded type parameter
      //  that will be inferred in subsequent code."
      val functor = new Functor[({ type lambda[beta] = A => beta })#lambda] {
        def map[A3, B](func: A => A3)(f: A3 => B): A => B = (a: A) => f(func(a))
      }
      functor.map(func)(f)
    }
  }

  test("SeqF OptionF") {
    val fii: Int => Int = i => i
    val fid: Int => Double = i => i * 0.5
    val fds: Double => String = d => d.toString

    assert(SeqF.map(List(1, 2, 3))(fii) == List(1, 2, 3))
    assert(SeqF.map(List(1, 2, 3))(fid) == List(0.5, 1, 1.5))

    //noinspection OptionEqualsSome
    assert(OptionF.map(Some(2))(fii) == Some(2))
    //noinspection OptionEqualsSome
    assert(OptionF.map(Some(2))(fid) == Some(1))

    val fa = FunctionF.map(fid)(fds)
    assert(fa(2) == "1.0")

    val fb = FunctionF.map[Int, Double, String](fid)(fds)
    assert(fb(2) == "1.0")

    val fc = fds compose fid
    assert(fc(2) == "1.0")

    val fd = fid andThen fds
    assert(fd(2) == "1.0")

  }
}
