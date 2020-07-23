import org.scalatest.funsuite.AnyFunSuite

class TypeTest extends AnyFunSuite {

  test("getClass") {
    assert(Nil.getClass.toString == "class scala.collection.immutable.Nil$")
    assert(Nil.getClass.getSimpleName == "Nil$")
    assert(Nil.getClass.getCanonicalName == "scala.collection.immutable.Nil$")


    assert(Int.getClass.toString == "class scala.Int$")
  }

  test("classOf") {
    assert(classOf[Int].toString == "int")
  }

}
