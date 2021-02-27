package rainko.bencode.parser

import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
import rainko.bencode.parser.StandardCharset._

sealed trait StandardCharset {

  final def underlying: Charset =
    this match {
      case `US-ASCII`   => StandardCharsets.US_ASCII
      case `ISO-8858-1` => StandardCharsets.ISO_8859_1
      case `UTF-8`      => StandardCharsets.UTF_8
      case `UTF-16BE`   => StandardCharsets.UTF_16BE
      case `UTF-16LE`   => StandardCharsets.UTF_16LE
      case `UTF-16`     => StandardCharsets.UTF_16
    }
}

object StandardCharset {
  case object `US-ASCII`   extends StandardCharset
  case object `ISO-8858-1` extends StandardCharset
  case object `UTF-8`      extends StandardCharset
  case object `UTF-16BE`   extends StandardCharset
  case object `UTF-16LE`   extends StandardCharset
  case object `UTF-16`     extends StandardCharset
}
