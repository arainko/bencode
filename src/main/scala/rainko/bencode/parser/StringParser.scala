package rainko.bencode.parser

import rainko.bencode.Bencode._
import rainko.bencode.Bencode
import rainko.bencode.BencodeError.ParsingFailure
import rainko.bencode.BencodeError

private[bencode] object StringParser extends Parser[String] {

  override def matchBInt(value: String): Boolean    = value.headOption.contains('i')
  override def matchBString(value: String): Boolean = value.headOption.exists(_.isDigit)
  override def matchBList(value: String): Boolean   = value.headOption.contains('l')
  override def matchBDict(value: String): Boolean   = value.headOption.contains('d')

  def parseBList(value: String) = {
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

  def parseBDict(value: String) = {
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
        case failedInput                                 => Left(BencodeError.parsingFailure("BDict", failedInput))
      }
    helper(value.drop(1), Nil)
  }

  def parseBInt(value: String) =
    value
      .drop(1)
      .takeWhile(_ != 'e')
      .toIntOption
      .map(BInt)
      .toRight(BencodeError.parsingFailure("BInt", value))

  def parseBString(string: String) = {
    val sizePart = string.takeWhile(_ != ':')
    for {
      size <-
        sizePart.toIntOption
          .toRight(BencodeError.parsingFailure("BString", string))
      text = string.drop(sizePart.length + 1).take(size)
    } yield BString(text)
  }

  def skipSize(bencode: Bencode): Int =
    bencode match {
      case BString(value) => value.length + value.length.toString.length + 1
      case BInt(value)    => value.toString.length + 2
      case BList(values)  => values.foldLeft(0)((acc, curr) => acc + skipSize(curr)) + 2
      case BDict(value)   => value.foldLeft(0)((acc, curr) => acc + skipSize(curr._1) + skipSize(curr._2)) + 2
    }

}
