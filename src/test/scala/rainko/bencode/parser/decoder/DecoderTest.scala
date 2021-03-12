package rainko.bencode.parser.decoder

import rainko.bencode.derivation.auto._
import rainko.bencode.{Bencode, Decoder}
import zio.test.Assertion._
import zio.test._

object DecoderTest extends DefaultRunnableSpec {

  sealed trait Coproduct

  final case class FirstCoprod(field1: Int, field2: Option[Int])        extends Coproduct
  final case class SecondCoprod(field1: String, field2: Option[String]) extends Coproduct

  final private case class Test(optional: Option[Int], notOptional: String)

  def spec: ZSpec[Environment, Failure] =
    suite("Decoders should")(
      test("decode a missing field as None") {
        val decoded = Bencode
          .fromFields(
            "notOptional" -> Bencode.fromString("abcd")
          )
          .cursor
          .as[Test]

        val expected = Test(None, "abcd")
        assert(decoded)(isRight(equalTo(expected)))
      },
      test("derive a decoder for an ADT with Decoder#or") {
        val firstCoprod = Bencode.fromFields(
          "field1" -> Bencode.fromInt(1)
        )

        val secondCoprod = Bencode.fromFields(
          "field1" -> Bencode.fromString("123456"),
          "field2" -> Bencode.fromString("yup")
        )

        val decoder = List[Decoder[Coproduct]](
          Decoder[FirstCoprod].widen,
          Decoder[SecondCoprod].widen
        ).reduce(_ or _)

        val decodedFirst  = decoder(firstCoprod)
        val decodedSecond = decoder(secondCoprod)

        assert(decodedFirst)(isRight(equalTo(FirstCoprod(1, None)))) &&
        assert(decodedSecond)(isRight(equalTo(SecondCoprod("123456", Some("yup")))))
      }
    )

}
