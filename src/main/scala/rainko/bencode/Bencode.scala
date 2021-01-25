package rainko.bencode

import scala.annotation.tailrec

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

  final def int: Option[BInt] =
    this match {
      case int: BInt => Some(int)
      case _         => None
    }

  final def string: Option[BString] =
    this match {
      case string: BString => Some(string)
      case _               => None
    }

  final def list: Option[BList] =
    this match {
      case list: BList => Some(list)
      case _           => None
    }

  final def dict: Option[BDict] =
    this match {
      case dict: BDict => Some(dict)
      case _           => None
    }

  final def decode[A: Decoder]: Either[String, A] = Decoder[A].apply(this)
}

object Bencode {
  final case class BString(value: String) extends Bencode
  final case class BInt(value: Int)       extends Bencode

  final case class BList(values: List[Bencode]) extends Bencode {
    override def toString: String = s"BList(${values.mkString(", ")})"
  }

  final case class BDict(fields: Map[BString, Bencode]) extends Bencode {

    def get(key: String): Option[Bencode] = fields.get(BString(key))

    def get[A: Decoder](key: String): Either[String, A] =
      fields
        .get(BString(key))
        .toRight(s"No key $key found!")
        .flatMap(_.decode)

  }

  object BDict {

    def apply(entries: (String, Bencode)*): BDict =
      BDict {
        entries.map { case (key, value) => (BString(key), value) }.toMap
      }
  }

  object BList {
    def apply(values: Bencode*): BList = BList(values.toList)
  }

  def parse(encoded: String): Option[Bencode] =
    encoded match {
      case int if int.headOption.contains('i')           => parseBInt(int)
      case string if string.headOption.exists(_.isDigit) => parseBString(string)
      case list if list.headOption.contains('l')         => parseBList(list)
      case dict if dict.headOption.contains('d')         => parseBDict(dict)
      case _                                             => None
    }

  private def parseBList(value: String): Option[BList] = {
    def helper(curr: String, acc: List[Bencode]): Option[BList] =
      curr match {
        case endList if endList.headOption.contains('e') => Some(BList(acc))
        case listElem =>
          parse(listElem).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            helper(curr.drop(skippedLength), acc :+ parsed)
          }
      }
    helper(value.drop(1), Nil)
  }

  private def parseBDict(value: String): Option[BDict] = {
    def helper(curr: String, acc: List[(BString, Bencode)]): Option[BDict] =
      curr match {
        case entry if entry.headOption.exists(_.isDigit) =>
          (for {
            label <- parseBString(entry)
            labelSize = skipSize(label)
            value <- parse(entry.drop(labelSize))
            valueSize = skipSize(value)
          } yield helper(entry.drop(labelSize + valueSize), acc :+ (label -> value))).flatten
        case endDict if endDict.headOption.contains('e') => Some(BDict(acc.toMap))
      }
    helper(value.drop(1), Nil)
  }

  private def parseBInt(value: String) =
    value.drop(1).takeWhile(_ != 'e').toIntOption.map(BInt)

  private def parseBString(string: String) = {
    val sizePart = string.takeWhile(_ != ':')
    for {
      size <- sizePart.toIntOption
      text = string.drop(sizePart.length + 1).take(size)
      checkedText <- Option.when(text.length == size)(text)
    } yield BString(checkedText)
  }

  private def skipSize(bencode: Bencode): Int =
    bencode match {
      case BString(value) => value.length + value.length.toString.length + 1
      case BInt(value)    => value.toString.length + 2
      case BList(values)  => values.foldLeft(0)((acc, curr) => acc + skipSize(curr)) + 2
      case BDict(value)   => value.foldLeft(0)((acc, curr) => acc + skipSize(curr._1) + skipSize(curr._2)) + 2
    }

}
