package rainko.bencode

import cats.instances.tuple._
import cats.syntax.bifunctor._
import rainko.bencode.BencodeError.ParsingFailure
import rainko.bencode.cursor.Cursor
import rainko.bencode.parser._
import rainko.bencode.syntax._
import scodec.bits.ByteVector

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.immutable.{Queue, SortedMap}

sealed trait Bencode {
  import Bencode._

  /**
   *    UTF-16, UTF-16LE and UTF-16BE charsets are known to cause issues
   */
  final def byteify(charset: Charset = StandardCharsets.UTF_8): ByteVector =
    this.deepDropEmpty match {
      case BString(value) =>
        val length     = ByteVector.view(value.length.toString.getBytes(charset))
        val sepratator = ByteVector.view(":".getBytes(charset))
        length ++ sepratator ++ value
      case BInt(value) =>
        val end   = ByteVector.view("e".getBytes(charset))
        val start = ByteVector.view("i".getBytes(charset))
        start ++ ByteVector.view(value.toString.getBytes(charset)) ++ end
      case BList(values) =>
        val start = ByteVector.view("l".getBytes(charset))
        val end   = ByteVector.view("e".getBytes(charset))
        start ++ values.foldLeft(ByteVector.empty)(_ ++ _.byteify(charset)) ++ end
      case BDict(fields) =>
        val start = ByteVector.view("d".getBytes(charset))
        val end   = ByteVector.view("e".getBytes(charset))
        val fieldReprs = fields
          .map { case (key, value) =>
            val labelLength = ByteVector.view(key.length.toString.getBytes(charset))
            val sepratator  = ByteVector.view(":".getBytes(charset))
            val label       = ByteVector.view(key.getBytes(charset))
            labelLength ++ sepratator ++ label ++ value.byteify(charset)
          }
          .foldLeft(ByteVector.empty)(_ ++ _)
        start ++ fieldReprs ++ end
      case BEmpty => ByteVector.empty
    }

  final def deepDropEmpty: Bencode =
    this match {
      case BDict(fields) =>
        BDict {
          fields
            .filter { case (_, value) => value != BEmpty }
            .map(_.bimap(identity, _.deepDropEmpty))
        }
      case other => other
    }

  final def transformFields(f: String => String): Bencode =
    this match {
      case BDict(fields) => BDict(fields.map(_.bimap(f, identity)))
      case other         => other
    }

  final def transformFieldsDeep(f: String => String): Bencode =
    this match {
      case BDict(fields) => BDict(fields.map(_.bimap(f, _.transformFieldsDeep(f))))
      case other         => other
    }

  final def cursor: Cursor = Cursor(this, Queue.empty)
}

object Bencode {
  final private[bencode] case class BString(value: ByteVector)                extends Bencode
  final private[bencode] case class BInt(value: Long)                         extends Bencode
  final private[bencode] case class BList(values: List[Bencode])              extends Bencode
  final private[bencode] case class BDict(fields: SortedMap[String, Bencode]) extends Bencode
  private[bencode] case object BEmpty                                         extends Bencode

  def fromString(string: String): Bencode = BString(string.getBytes.toByteVector)

  def fromByteArray(array: Array[Byte]): Bencode = BString(ByteVector(array))

  def fromByteVector(vector: ByteVector): Bencode = BString(vector)

  def fromInt(int: Int): Bencode = BInt(int.toLong)

  def fromValues(values: Bencode*): Bencode = BList(values.toList)

  def fromSequence(seq: Seq[Bencode]): Bencode = BList(seq.toList)

  def fromFields(fields: (String, Bencode)*): Bencode = Bencode.fromMap(SortedMap(fields: _*))

  def fromMap(entries: Map[String, Bencode]): Bencode = BDict(SortedMap.from(entries))

  /*
    UTF-16, UTF-16LE and UTF-16BE charsets are known to cause issues
   */
  def parse(
    encoded: String,
    charset: Charset
  ): Either[ParsingFailure, Bencode] =
    ByteParser
      .withCharset(charset)
      .parse(encoded.getBytes(charset).toByteVector)

  /*
    UTF-16, UTF-16LE and UTF-16BE charsets are known to cause issues
   */
  def parse(
    encoded: ByteVector,
    charset: Charset = StandardCharsets.UTF_8
  ): Either[ParsingFailure, Bencode] =
    ByteParser
      .withCharset(charset)
      .parse(encoded)

}
