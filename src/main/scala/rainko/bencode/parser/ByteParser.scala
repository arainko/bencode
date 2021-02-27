package rainko.bencode.parser

import scodec.bits.ByteVector
import rainko.bencode._
import rainko.bencode.Bencode._
import rainko.bencode.BencodeError._
import rainko.bencode.syntax._
import java.nio.charset.StandardCharsets

object ByteParser extends Parser[ByteVector] {

  implicit private val charset = StandardCharsets.UTF_8

  private val intBegin  = "i".getBytes.head
  private val listBegin = "l".getBytes.head
  private val dictBegin = "d".getBytes.head
  private val end       = "e".getBytes.head

  private val stringLengthSeparator = ":".getBytes.head
  private def isNumeric(byte: Byte) = byte >= 48 && byte <= 57

  private def parsingFailure(typeName: String, bytes: ByteVector) =
    BencodeError.parsingFailure(typeName, s"${bytes.take(15).parseWithCharset}...")

  override def matchBInt(value: ByteVector): Boolean    = value.headOption.contains(intBegin)
  override def matchBString(value: ByteVector): Boolean = value.headOption.exists(isNumeric)
  override def matchBList(value: ByteVector): Boolean   = value.headOption.contains(listBegin)
  override def matchBDict(value: ByteVector): Boolean   = value.headOption.contains(dictBegin)

  private def skipSize(bencode: Bencode): Long =
    bencode match {
      case BString(value) => 
        val sizePartLength = value.length.toString.length + 1
        value.length + sizePartLength
      case BInt(value) => value.toString.length.toLong + 2
      case BDict(fields) => fields.foldLeft(0L)((acc, curr) => acc + labelSkipSize(curr._1) + skipSize(curr._2)) + 2
      case BList(values) => values.foldLeft(0L)((acc, curr) => acc + skipSize(curr)) + 2
    }

    def labelSkipSize(label: String): Int = label.length.toString.size + label.length + 1

  override def parseBInt(value: ByteVector): Either[ParsingFailure, BInt] =
    value
      .drop(1)
      .takeWhile(_ != end)
      .parseWithCharset
      .flatMap(_.toLongOption.toRight(parsingFailure("BInt", value)))
      .map(BInt)

  override def parseBString(value: ByteVector): Either[ParsingFailure, BString] = {
    val stringLengthPart = value.takeWhile(_ != stringLengthSeparator)
    for {
      stringLength <- stringLengthPart.parseWithCharset
      length <- stringLength.toLongOption.toRight(parsingFailure("BString", value))
      bstring = value.drop(stringLengthPart.length + 1).take(length)
    } yield BString(bstring)
  }

  override def parseBList(value: ByteVector): Either[ParsingFailure, BList] = {
    def accumulate(curr: ByteVector, acc: List[Bencode]): Either[ParsingFailure, BList] = {
      curr match {
        case endList if endList.headOption.contains(end) => Right(BList(acc))
        case listElem =>
          parse(listElem).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            accumulate(curr.drop(skippedLength), acc :+ parsed)
          }
      }
    }
    accumulate(value.drop(1), Nil)
  }
    
  override def parseBDict(value: ByteVector): Either[ParsingFailure, BDict] = {
    def accumulate(curr: ByteVector, acc: List[(String, Bencode)]): Either[ParsingFailure, BDict] =
      curr match {
        case entry if matchBString(entry) =>
          for {
            label <- parseBString(entry)
            labelSize = skipSize(label)
            stringLabel <- label.value.parseWithCharset
            value <- parse(entry.drop(labelSize))
            valueSize = skipSize(value)
            nextField <- accumulate(entry.drop(labelSize + valueSize), acc :+ (stringLabel -> value))
          } yield nextField
        case endDict if endDict.headOption.contains(end) => Right(BDict(acc.toMap))
        case failedInput => Left(parsingFailure("BDict", failedInput))
      }
    accumulate(value.drop(1), Nil)
  }

}
