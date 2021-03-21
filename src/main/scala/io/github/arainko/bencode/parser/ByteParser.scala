package io.github.arainko.bencode.parser

import io.github.arainko.bencode.Bencode._
import io.github.arainko.bencode.BencodeError._
import io.github.arainko.bencode._
import io.github.arainko.bencode.util._
import scodec.bits.ByteVector

import java.nio.charset.Charset
import scala.collection.immutable.SortedMap

private[bencode] object ByteParser {
  def withCharset(charset: Charset): ByteParser = new ByteParser(charset)
}

private[parser] class ByteParser(val charset: Charset) {

  implicit private val parsingCharset: Charset = charset

  private val intStart              = "i".bytesWithCharset.head
  private val listStart             = "l".bytesWithCharset.head
  private val dictStart             = "d".bytesWithCharset.head
  private val stringLengthSeparator = ":".bytesWithCharset.head
  private val end                   = "e".bytesWithCharset.head

  private val upperNumericLimit = "9".bytesWithCharset.head
  private val lowerNumericLimit = "0".bytesWithCharset.head

  private def isNumeric(byte: Byte) = byte >= lowerNumericLimit && byte <= upperNumericLimit

  private def parsingFailure(typeName: String, bytes: ByteVector) =
    BencodeError.parsingFailure(
      s"$typeName (using charset ${charset.displayName}",
      s"${bytes.take(15).toHex}..."
    )

  private def matchBInt(value: ByteVector): Boolean    = value.headOption.contains(intStart)
  private def matchBString(value: ByteVector): Boolean = value.headOption.exists(isNumeric)
  private def matchBList(value: ByteVector): Boolean   = value.headOption.contains(listStart)
  private def matchBDict(value: ByteVector): Boolean   = value.headOption.contains(dictStart)

  private def skipSize(bencode: Bencode): Long =
    bencode match {
      case BString(value) =>
        val sizePartLength = value.length.toString.length + 1
        value.length + sizePartLength
      case BInt(value)   => value.toString.length.toLong + 2
      case BDict(fields) => fields.foldLeft(0L)((acc, curr) => acc + labelSkipSize(curr._1) + skipSize(curr._2)) + 2
      case BList(values) => values.foldLeft(0L)((acc, curr) => acc + skipSize(curr)) + 2
      case BEmpty        => 0
    }

  private def labelSkipSize(label: String): Int = label.length.toString.size + label.length + 1

  private def parseBInt(value: ByteVector): Either[ParsingFailure, BInt] =
    value
      .drop(1)
      .takeWhile(_ != end)
      .parseWithCharset
      .flatMap(_.toLongOption.toRight(parsingFailure("BInt", value)))
      .map(BInt)

  private def parseBString(value: ByteVector): Either[ParsingFailure, BString] = {
    val stringLengthPart = value.takeWhile(_ != stringLengthSeparator)
    for {
      stringLength <- stringLengthPart.parseWithCharset
      length       <- stringLength.toLongOption.toRight(parsingFailure("BString", value))
      bstring = value.drop(stringLengthPart.length + 1).take(length)
    } yield BString(bstring)
  }

  private def parseBList(value: ByteVector): Either[ParsingFailure, BList] = {
    def accumulate(curr: ByteVector, acc: List[Bencode]): Either[ParsingFailure, BList] =
      curr match {
        case endList if endList.headOption.contains(end) => Right(BList(acc))
        case listElem =>
          parse(listElem).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            accumulate(curr.drop(skippedLength), acc :+ parsed)
          }
      }
    accumulate(value.drop(1), Nil)
  }

  private def parseBDict(value: ByteVector): Either[ParsingFailure, BDict] = {
    def accumulate(curr: ByteVector, acc: List[(String, Bencode)]): Either[ParsingFailure, BDict] =
      curr match {
        case entry if matchBString(entry) =>
          for {
            label <- parseBString(entry)
            labelSize = skipSize(label)
            stringLabel <- label.value.parseWithCharset
            value       <- parse(entry.drop(labelSize))
            valueSize = skipSize(value)
            nextField <- accumulate(entry.drop(labelSize + valueSize), acc :+ (stringLabel -> value))
          } yield nextField
        case endDict if endDict.headOption.contains(end) => Right(BDict(SortedMap.from(acc)))
        case failedInput                                 => Left(parsingFailure("BDict", failedInput))
      }
    accumulate(value.drop(1), Nil)
  }

  final def parse(value: ByteVector): Either[BencodeError.ParsingFailure, Bencode] =
    value match {
      case bint if matchBInt(bint)          => parseBInt(bint)
      case bstring if matchBString(bstring) => parseBString(bstring)
      case blist if matchBList(blist)       => parseBList(blist)
      case bdict if matchBDict(bdict)       => parseBDict(bdict)
      case _                                => Left(parsingFailure("Bencode", value))
    }

}
