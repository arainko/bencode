package rainko.bencode.encoder

import rainko.bencode._
import rainko.bencode.derivation.semiauto._
import rainko.bencode.syntax._
import zio.test.Assertion._
import zio.test._

object EncoderTest extends DefaultRunnableSpec {

  final case class OptionTest(optional: Option[Int], nonOpt: String)

  private val testGen = for {
    optional <- Gen.option(Gen.anyInt)
    nonOpt   <- Gen.anyASCIIString
  } yield OptionTest(optional, nonOpt)

  implicit val encoder = deriveEncoder[OptionTest]
  implicit val decoder = deriveDecoder[OptionTest]

  def spec: ZSpec[Environment, Failure] =
    suite("Derived encoders should")(
      testM("encode and decode back") {
        check(testGen) { opt =>
          val decoded = opt.encode.cursor.as[OptionTest]
          assert(decoded)(isRight(equalTo(opt)))
        }
      },
      testM("encode and decode from byte representation") {
        check(testGen) { opt =>
          val decoded = Bencode.parse(opt.encode.byteify()).flatMap(_.cursor.as[OptionTest])
          assert(decoded)(isRight(equalTo(opt)))
        }
      }
    )
}
