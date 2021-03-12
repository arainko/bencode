package rainko.bencode.parser.decoder

import rainko.bencode.Decoder
import rainko.bencode.derivation.semiauto._

object SemiautoDecoderTest {
  final case class FirstCoprod(field1: Int, field2: Option[Int])

  val cos: Decoder[FirstCoprod] = deriveDecoder[FirstCoprod]
}
