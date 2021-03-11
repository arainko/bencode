package rainko.bencode.parser.decoder

import zio._
import zio.test._
import zio.test.Assertion._
import rainko.bencode.derivation.auto._
import rainko.bencode.Bencode
import rainko.bencode.Decoder

object DecoderTest extends DefaultRunnableSpec {

  final private case class Test(optional: Option[Int], notOptional: String)

  def spec: ZSpec[Environment, Failure] =
    suite("Decoders should")(
      test("decode a missing field as None") {
        val decoded = Bencode
          .fromFields(
            "notOptional" -> Bencode.fromString("abcd")
          ).cursor.as[Test]

        val expected = Test(None, "abcd")
        assert(decoded)(isRight(equalTo(expected)))
      }
    )

}
