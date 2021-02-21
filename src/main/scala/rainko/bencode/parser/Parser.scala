package rainko.bencode.parser

import rainko.bencode.Bencode
import rainko.bencode.BencodeError
import rainko.bencode.Bencode._

abstract private[bencode] class Parser[A] {
  def matchBInt(value: A): Boolean
  def matchBString(value: A): Boolean
  def matchBList(value: A): Boolean
  def matchBDict(value: A): Boolean

  def parseBInt(value: A): Either[BencodeError.ParsingFailure, BInt]
  def parseBString(value: A): Either[BencodeError.ParsingFailure, BString]
  def parseBList(value: A): Either[BencodeError.ParsingFailure, BList]
  def parseBDict(value: A): Either[BencodeError.ParsingFailure, BDict]

  final def parse(value: A): Either[BencodeError.ParsingFailure, Bencode] =
    value match {
      case bint if matchBInt(bint)          => parseBInt(bint)
      case bstring if matchBString(bstring) => parseBString(bstring)
      case blist if matchBList(blist)       => parseBList(blist)
      case bdict if matchBDict(bdict)       => parseBDict(bdict)
      case _                                => Left(BencodeError.parsingFailure("Bencode", value.toString))
    }
}
