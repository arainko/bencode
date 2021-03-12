package rainko.bencode.parser.decoder

import rainko.bencode.derivation.semiauto._
import rainko.bencode.Decoder

object SemiautoDecoderTest {
  final case class FirstCoprod(field1: Int, field2: Option[Int])

  val cos: Decoder[FirstCoprod] = deriveDecoder[FirstCoprod]
}
