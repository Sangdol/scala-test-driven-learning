import org.scalatest.funsuite.AnyFunSuite

/**
  * Stream is deprecated and replaced by LazyList at 2.13.
  */
class LazyListStreamTest extends AnyFunSuite {

  test("basic") {
    val list = 1 :: 2 :: 3 :: Nil
    val lazyList = 1 #:: 2 #:: 3 #:: LazyList.empty

    assert(list == lazyList)
  }

}
