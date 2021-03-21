package io.github.arainko.bencode.encoder

import io.github.arainko.bencode._
import io.github.arainko.bencode.derivation.semiauto._
import io.github.arainko.bencode.syntax._
import zio.test.Assertion._
import zio.test._

object EncoderTest extends DefaultRunnableSpec {

  final case class OptionTest(optional: Option[Int], nonOpt: String)

  private val testGen = for {
    optional <- Gen.option(Gen.anyInt)
    nonOpt   <- Gen.anyASCIIString
  } yield OptionTest(optional, nonOpt)

  implicit val encoder: Encoder.AsObject[OptionTest] = deriveEncoder[OptionTest]
  implicit val decoder: Decoder[OptionTest]          = deriveDecoder[OptionTest]

  def spec: ZSpec[Environment, Failure] =
    suite("Derived encoders should")(
      testM("encode and decode back") {
        check(testGen) { opt =>
          val decoded = opt.asBencode.cursor.as[OptionTest]
          assert(decoded)(isRight(equalTo(opt)))
        }
      },
      testM("encode and decode from byte representation") {
        check(testGen) { opt =>
          val decoded = Bencode.parse(opt.asBencode.byteify()).flatMap(_.cursor.as[OptionTest])
          assert(decoded)(isRight(equalTo(opt)))
        }
      }
    )
}
