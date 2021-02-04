package rainko.bencode

import rainko.bencode.BencodeError.ParsingFailure
import rainko.bencode.cursor.Cursor

import scala.collection.immutable.Queue

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

  def fromMap(entries: Map[String, Bencode]): BDict =
    BDict(entries.map { case (key, value) => (BString(key), value) })

  def parse(encoded: String): Either[ParsingFailure, Bencode] =
    encoded match {
      case int if int.headOption.contains('i')           => parseBInt(int)
      case string if string.headOption.exists(_.isDigit) => parseBString(string)
      case list if list.headOption.contains('l')         => parseBList(list)
      case dict if dict.headOption.contains('d')         => parseBDict(dict)
      case _                                             => Left(BencodeError.parsingFailure("Bencode", encoded))
    }

  private def parseBList(value: String) = {
    def helper(curr: String, acc: List[Bencode]): Either[ParsingFailure, BList] =
      curr match {
        case endList if endList.headOption.contains('e') => Right(BList(acc))
        case listElem =>
          parse(listElem).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            helper(curr.drop(skippedLength), acc :+ parsed)
          }
      }
    helper(value.drop(1), Nil)
  }

  private def parseBDict(value: String) = {
    def helper(curr: String, acc: List[(BString, Bencode)]): Either[ParsingFailure, BDict] =
      curr match {
        case entry if entry.headOption.exists(_.isDigit) =>
          for {
            label <- parseBString(entry)
            labelSize = skipSize(label)
            value <- parse(entry.drop(labelSize))
            valueSize = skipSize(value)
            nextField <- helper(entry.drop(labelSize + valueSize), acc :+ (label -> value))
          } yield nextField
        case endDict if endDict.headOption.contains('e') => Right(BDict(acc.toMap))
      }
    helper(value.drop(1), Nil)
  }

  private def parseBInt(value: String) =
    value
      .drop(1)
      .takeWhile(_ != 'e')
      .toIntOption
      .map(BInt)
      .toRight(BencodeError.parsingFailure("BInt", value))

  private def parseBString(string: String) = {
    val sizePart = string.takeWhile(_ != ':')
    for {
      size <-
        sizePart.toIntOption
          .toRight(BencodeError.parsingFailure("BString", string))
      text = string.drop(sizePart.length + 1).take(size)
    } yield BString(text)
  }

  private def skipSize(bencode: Bencode): Int =
    bencode match {
      case BString(value) => value.length + value.length.toString.length + 1
      case BInt(value)    => value.toString.length + 2
      case BList(values)  => values.foldLeft(0)((acc, curr) => acc + skipSize(curr)) + 2
      case BDict(value)   => value.foldLeft(0)((acc, curr) => acc + skipSize(curr._1) + skipSize(curr._2)) + 2
    }

}
