package rainko.bencode

import rainko.bencode.BencodeError.ParsingFailure
import rainko.bencode.cursor.Cursor
import rainko.bencode.parser._
import rainko.bencode.syntax._
import scodec.bits.ByteVector

import java.nio.charset.Charset
import scala.collection.immutable.Queue

sealed trait Bencode {
  import Bencode._

  final def stringify: String = this.stringifyUsingCharset(Charset.defaultCharset)

  final def stringifyUsingCharset(charset: StandardCharset): String = this.stringifyUsingCharset(charset.underlying)

  final def stringifyUsingCharset(charset: Charset): String =
    this match {
      case BString(value) => s"${value.length}:${value.toArray.stringUsingCharset(charset)}"
      case BInt(value)    => s"i${value}e"
      case BList(values)  => s"l${values.map(_.stringifyUsingCharset(charset)).mkString}e"
      case BDict(values) =>
        values
          .map { case (key, value) => s"${key.length}:${key}${value.stringifyUsingCharset(charset)}" }
          .mkString("d", "", "e")
    }

  final def prettyStringify: String = {
    def withIndent(level: Int, bencode: Bencode): String =
      bencode match {
        case string @ BString(_) => "  " + string.stringify
        case int @ BInt(_)       => "  " + int.stringify
        case BList(values) =>
          values
            .map(v => withIndent(level + 1, v))
            .mkString(s"\n${"  " * level}l\n", "\n", s"\n${"  " * level}e")
        case BDict(fields) =>
          fields
            .map { case (key, value) =>
              s"${"  " * level}${key.length}:${key}${withIndent(level + 1, value)}"
            }
            .mkString(s"\n${"  " * level}d\n", "\n", s"\n${"  " * level}e")
      }
    withIndent(0, this)
  }

  final def cursor: Cursor = Cursor(this, Queue.empty)
}

object Bencode {
  final case class BString(value: ByteVector)          extends Bencode
  final case class BInt(value: Long)                   extends Bencode
  final case class BList(values: List[Bencode])        extends Bencode
  final case class BDict(fields: Map[String, Bencode]) extends Bencode

  def fromString(string: String): BString = BString(string.getBytes.toByteVector)

  def fromInt(int: Int): BInt = BInt(int.toLong)

  def fromValues(values: Bencode*): BList = BList(values.toList)

  def fromSequence(seq: Seq[Bencode]): BList = BList(seq.toList)

  def fromFields(fields: (String, Bencode)*): BDict = Bencode.fromMap(fields.toMap)

  def fromMap(entries: Map[String, Bencode]): BDict = BDict(entries)

  def parse(encoded: String): Either[ParsingFailure, Bencode] = ByteParser.default.parse(encoded.getBytes.toByteVector)

  def parse(encoded: ByteVector): Either[ParsingFailure, Bencode]      = ByteParser.default.parse(encoded)
  def parseUsingCharset(encoded: ByteVector, charset: StandardCharset): Either[ParsingFailure,Bencode] = ByteParser(charset).parse(encoded)

  def parseUsingJavaCharset(encoded: ByteVector, charset: Charset): Either[ParsingFailure,Bencode] = ByteParser.fromJavaCharset(charset).parse(encoded)

}
