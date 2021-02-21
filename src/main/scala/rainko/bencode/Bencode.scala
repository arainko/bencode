package rainko.bencode

import rainko.bencode.BencodeError.ParsingFailure
import rainko.bencode.cursor.Cursor

import scala.collection.immutable.Queue
import rainko.bencode.Bencode.BDict
import rainko.bencode.Bencode.BString
import rainko.bencode.Bencode.BInt
import rainko.bencode.Bencode.BList
import rainko.bencode.parser._

sealed trait Bencode {
  import Bencode._

  final def stringify: String =
    this match {
      case BString(value) => s"${value.length}:$value"
      case BInt(value)    => s"i${value}e"
      case BList(values)  => s"l${values.map(_.stringify).mkString}e"
      case BDict(values) =>
        values
          .map { case (key, value) => s"${key.stringify}${value.stringify}" }
          .mkString("d", "", "e")
    }

  final def prettyStringify: String = {
    def withIndent(level: Int, bencode: Bencode): String =
      bencode match {
        case string @ BString(value) => "  " + string.stringify
        case int @ BInt(value)       => "  " + int.stringify
        case list @ BList(values) =>
          values
            .map(v => withIndent(level + 1, v))
            .mkString(s"\n${"  " * level}l\n", "\n", s"\n${"  " * level}e")
        case dict @ BDict(fields) =>
          fields
            .map { case (key, value) =>
              s"${"  " * level}${key.stringify}${withIndent(level + 1, value)}"
            }
            .mkString(s"\n${"  " * level}d\n", "\n", s"\n${"  " * level}e")
      }
    withIndent(0, this)
  }

  final def cursor: Cursor = Cursor(this, Queue.empty)
}

object Bencode {
  final case class BString(value: String)               extends Bencode
  final case class BInt(value: Int)                     extends Bencode
  final case class BList(values: List[Bencode])         extends Bencode
  final case class BDict(fields: Map[BString, Bencode]) extends Bencode

  def fromString(string: String): BString = BString(string)

  def fromInt(int: Int): BInt = BInt(int)

  def fromValues(values: Bencode*): BList = BList(values.toList)

  def fromSequence(seq: Seq[Bencode]): BList = BList(seq.toList)

  def fromFields(fields: (String, Bencode)*): BDict = Bencode.fromMap(fields.toMap)

  def fromMap(entries: Map[String, Bencode]): BDict = BDict(entries.map { case (key, value) => (BString(key), value) })

  def parse(encoded: String): Either[ParsingFailure, Bencode] = StringParser.parse(encoded)

}
