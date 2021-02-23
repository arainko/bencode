package rainko.bencode.parser

import scodec.bits.ByteVector
import rainko.bencode._
import rainko.bencode.Bencode._
import rainko.bencode.BencodeError._

object ByteParser extends Parser[ByteVector] {

  private val intBegin  = "i".getBytes.head
  private val listBegin = "l".getBytes.head
  private val dictBegin = "d".getBytes.head
  private val end       = "e".getBytes.head

  private val stringLengthSeparator = ":".getBytes.head
  private def isNumeric(byte: Byte) = byte >= 48 && byte <= 57

  override def matchBInt(value: ByteVector): Boolean    = value.headOption.contains(intBegin)
  override def matchBString(value: ByteVector): Boolean = value.headOption.exists(isNumeric)
  override def matchBList(value: ByteVector): Boolean   = value.headOption.contains(listBegin)
  override def matchBDict(value: ByteVector): Boolean   = value.headOption.contains(dictBegin)

  override def parseBInt(value: ByteVector): Either[ParsingFailure, BInt] =
    value
      .drop(1)
      .takeWhile(_ != end)
      .decodeUtf8
      .left
      .map(_ => BencodeError.parsingFailure("BInt", "Bytes..."))
      .flatMap(_.toIntOption.toRight(BencodeError.parsingFailure("BInt", "Bytes...")))
      .map(BInt)

  override def parseBString(value: ByteVector): Either[ParsingFailure, BString] = {
    val length = value.takeWhile(isNumeric)
    for {
        length <- value.takeWhile(isNumeric).decodeUtf8.left.map(_ => BencodeError.parsingFailure("BInt", "Bytes..."))
        takeForm = value.drop(length.length + 1).take()
    } yield 
  }

  override def parseBList(value: ByteVector): Either[ParsingFailure, BList] = ???

  override def parseBDict(value: ByteVector): Either[ParsingFailure, BDict] = ???

}
