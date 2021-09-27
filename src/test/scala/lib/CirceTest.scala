package lib

import io.circe.generic.extras.Configuration
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import io.circe._
import org.scalatest.funsuite.AnyFunSuite // Encoding and Decoding

/**
  * https://github.com/circe/circe
  */
class CirceTest extends AnyFunSuite {

  // https://circe.github.io/circe/parsing.html
  test("parsing") {
    val rawJson: String = """
        {
          "foo": "bar",
          "baz": 123,
          "list of stuff": [ 4, 5, 6 ]
        }
        """

    val parseResult = parse(rawJson)
    // parseResult: Either[ParsingFailure, Json] = Right(
    //   value = JObject(
    //     value = object[foo -> "bar",baz -> 123,list of stuff -> [
    //   4,
    //   5,
    //   6
    // ]]
    //   )
    // )

    // How to deal with JObject? You can use Json
    assert(parseResult.getOrElse("{}").getClass.getSimpleName == "JObject")

    val json: Json = parseResult.getOrElse(Json.Null)
    assert(json.isObject)
  }

  test("parsing fail") {
    val parseResult = parse("yolo")

    // res0: Either[ParsingFailure, Json] = Left(
    //   value = ParsingFailure(
    //     message = "expected json value got 'yolo' (line 1, column 1)",
    //     underlying = ParseException(
    //       msg = "expected json value got 'yolo' (line 1, column 1)",
    //       index = 0,
    //       line = 1,
    //       col = 1
    //     )
    //   )
    // )

    assert(parseResult.isLeft)
    assert(
      parseResult.left.getOrElse("").getClass.getSimpleName == "ParsingFailure"
    )
  }

  // https://circe.github.io/circe/cursors.html
  test("Traversing JSON") {
    val json: String = """
          {
            "id": "c730433b-082c-4984-9d66-855c243266f0",
            "name": "Foo",
            "counts": [1, 2, 3],
            "values": {
              "bar": true,
              "baz": 100.001,
              "qux": ["a", "b"]
            }
          }
        """

    val doc: Json = parse(json).getOrElse(Json.Null)
    val cursor: HCursor = doc.hcursor

    val baz: Decoder.Result[Double] =
      cursor.downField("values").downField("baz").as[Double]

    // shortcut
    val baz2: Decoder.Result[Double] =
      cursor.downField("values").get[Double]("baz")

    assert(baz == Right(value=100.001))
    assert(baz2 == Right(value=100.001))

    // why second?
    val secondQux: Decoder.Result[String] =
      cursor.downField("values").downField("qux").downArray.as[String]

    assert(secondQux == Right(value="a"))

    val qux: Decoder.Result[List[String]] =
      cursor.downField("values").downField("qux").as[List[String]]

    assert(qux == Right(value=List("a", "b")))

    val name = cursor.downField("name").as[String]
    assert(name == Right("Foo"))
  }

  // https://circe.github.io/circe/cursors.html#transforming-data
  test("Transforming data") {
    val json: String = """
          {
            "id": "c730433b-082c-4984-9d66-855c243266f0",
            "name": "Foo",
            "counts": [1, 2, 3],
            "values": {
              "bar": true,
              "baz": 100.001,
              "qux": ["a", "b"]
            }
          }
        """

    val doc: Json = parse(json).getOrElse(Json.Null)

    // What does HCursor and ACursor mean?
    // https://circe.github.io/circe/cursors.html#cursors
    // * Cursor provides functionality for moving around a tree and making modifications
    // * HCursor tracks the history of operations performed. This can be used to provide useful error messages when something goes wrong.
    // * ACursor also tracks history, but represents the possibility of failure (e.g. calling downField on a field that doesn’t exist)
    val cursor: HCursor = doc.hcursor
    val reversedNameCursor: ACursor =
      cursor.downField("name").withFocus(_.mapString(_.reverse))

    // This doesn't seem right...
    // Maybe this is better https://circe.github.io/circe/optics.html
    val reversedTop: Option[Json] = reversedNameCursor.top
    val reversedNameJson: Json = reversedTop.getOrElse(Json.Null)

    val reversedNameJsonCursor: HCursor = reversedNameJson.hcursor
    val reversedName = reversedNameJsonCursor.get[String]("name")
    assert(reversedName == Right("ooF"))
  }

  test("Encoding and decoding") {
    // Encoder[A]: A -> Json
    // Decoder[A]: Json -> (Exception, A)

    // Type Class Pattern (with implicit classes)
    val intsJson = List(1, 2, 3).asJson
    val result = """[
                   |  1,
                   |  2,
                   |  3
                   |]""".stripMargin

    assert(intsJson.toString() == result)
    assert(intsJson.as[List[Int]] == Right(List(1, 2, 3)))

    // Decoding
    assert(decode[List[Int]]("[1,2,3]") == Right(List(1,2,3)))
  }

  test("Semi-automatic Derivation") {
    case class Child(name: String)

    implicit val childDecoder: Decoder[Child] = deriveDecoder
    implicit val childEncoder: Encoder[Child] = deriveEncoder

    val c = Child("c")

    val result = """{
                   |  "name" : "c"
                   |}""".stripMargin

    assert(c.asJson.toString() == result)
  }

  test("Semi-automatic Derivation - generic extras") {
    import io.circe.generic.extras.semiauto._

    case class Child(name: String)

    implicit val encoderCreator: Encoder[Child] = deriveUnwrappedEncoder
    implicit val decoderCreator: Decoder[Child] = deriveUnwrappedDecoder

    val c = Child("c")

    assert(c.asJson.toString() == "\"c\"")
  }

  test("@JsonCodec") {
//    import io.circe.generic.JsonCodec
//
//    @JsonCodec case class Bar(i: Int, S: String)

    // Exception
    // https://circe.github.io/circe/codecs/semiauto-derivation.html#jsoncodec
    // -Ymacro-annotations flag (?)
    //   could not find implicit value for parameter encoder: io.circe.Encoder[Bar]
    //   val json: Json = Bar(10, "B").asJson
//    val json: Json = Bar(10, "B").asJson
//    assert(json.hcursor.downField("i").as[Int] == Right(10))
  }

  test("Automatic Derivation") {
    import io.circe.generic.auto._
    case class Child(name: String)
    case class Parent(children: Seq[Child])

    val p = Parent(Seq(Child("c1"), Child("c2")))
    val result = """{
                   |  "children" : [
                   |    {
                   |      "name" : "c1"
                   |    },
                   |    {
                   |      "name" : "c2"
                   |    }
                   |  ]
                   |}""".stripMargin

    assert(p.asJson.toString() == result)
  }

  /**
    * https://github.com/circe/circe-generic-extras
    */
  test("circe generic extras from document") {
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.semiauto._

    case class Foo(fooBar: String)

    implicit val customConfig: Configuration = Configuration.default.withKebabCaseMemberNames

    implicit val fooEncoder: Encoder[Foo] = deriveConfiguredEncoder
    implicit val fooDecoder: Decoder[Foo] = deriveConfiguredDecoder

    val result = """{
                   |  "foo-bar" : "a"
                   |}""".stripMargin

    assert(Foo("a").asJson.toString() == result)

    // JSON to Foo
    assert(Foo("a").asJson.as[Foo].isRight)

    // Not a very good way to decode (Double rights)
    assert(parse(result).map(f => f.as[Foo]).isRight)

    // Either --map--> double Either
    val fooOption = parse(result).map(f => f.as[Foo]) match {
      case Right(Right(foo)) => Option(foo)
      case _ => None
    }
    assert(fooOption == Option(Foo("a")))

    // https://edward-huang.com/scala/tech/soft-development/etl/circe/2019/11/28/6-quick-tips-to-parse-json-with-circe/
    val fooOption2 = decode[Foo](result) match {
      case Right(foo) => Option(foo)
      case _ => None
    }
    assert(fooOption2 == Option(Foo("a")))
  }

  // Stackoverflow
  test("circe generic extras 1") {
    import io.circe.generic.extras.semiauto._

    // These prevents this exception.
    //   could not find Lazy implicit value of type io.circe.generic.extras.encoding.ConfiguredAsObjectEncoder[Parent]
    //   implicit val encoder: Encoder[Parent] = deriveConfiguredEncoder
    implicit val conf: Configuration = Configuration.default.withSnakeCaseMemberNames

    case class Child(name: String)
    case class Parent(children: Seq[Child])

    // These prevents this exception.
    //   could not find Lazy implicit value of type io.circe.generic.extras.encoding.ConfiguredAsObjectEncoder[Parent]
    //   implicit val encoder: Encoder[Parent] = deriveConfiguredEncoder
    implicit val encoderCreator: Encoder[Child] = deriveUnwrappedEncoder
    implicit val decoderCreator: Decoder[Child] = deriveUnwrappedDecoder

    implicit val encoder: Encoder[Parent] = deriveConfiguredEncoder
    implicit val decoder: Decoder[Parent] = deriveConfiguredDecoder

    val p = Parent(Seq(Child("c1"), Child("c2")))
    val json = """{
                 |  "children" : [
                 |    "c1",
                 |    "c2"
                 |  ]
                 |}""".stripMargin

    // Where is asJson?
    //   In Encoder (Type Class Pattern (with implicit classes))
    //   import io.circe.syntax.EncoderOps is needed.
    // https://stackoverflow.com/questions/51671405/how-can-i-configure-circe-to-stop-using-nested-class-names-as-key-names-in-encod
    assert(p.asJson.toString() == json)
  }

  test("circe generic extras 2") {
    import io.circe.generic.extras.semiauto._

    implicit val conf: Configuration = Configuration.default.withSnakeCaseMemberNames

    case class Child(name: String)
    case class Parent(children: Seq[Child])

    implicit val encoderCreator: Encoder[Child] = deriveUnwrappedEncoder
    implicit val decoderCreator: Decoder[Child] = deriveUnwrappedDecoder

    implicit val encoder: Encoder[Parent] = deriveUnwrappedEncoder
    implicit val decoder: Decoder[Parent] = deriveUnwrappedDecoder

    val p = Parent(Seq(Child("c1"), Child("c2")))
    val json = """[
                 |  "c1",
                 |  "c2"
                 |]""".stripMargin

    assert(p.asJson.toString() == json)

    // Wrong way to decode
    assert(json.asJson.as[Parent].isLeft)

    // Right way to decode
    assert(decode[Parent](json).getOrElse(None) == p)
  }

}
