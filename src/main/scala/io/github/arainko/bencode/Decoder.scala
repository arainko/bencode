package io.github.arainko.bencode

import cats.instances.list._
import cats.syntax.all._
import io.github.arainko.bencode.Bencode.BEmpty
import io.github.arainko.bencode.BencodeError._
import scodec.bits.ByteVector

import java.time.{Instant, LocalDate, ZoneId}

trait Decoder[A] { self =>
  def apply(bencode: Bencode): Either[DecodingError, A]

  final def map[B](f: A => B): Decoder[B] = bencode => self.apply(bencode).map(f)

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] =
    bencode =>
      self.apply(bencode) match {
        case failed @ Left(_) => failed.asInstanceOf[Either[DecodingError, B]]
        case Right(value)     => f(value)(bencode)
      }

  final def or[AA >: A](other: Decoder[AA]): Decoder[AA] = bencode => self.apply(bencode).orElse(other.apply(bencode))

  final def widen[B >: A]: Decoder[B] = this.asInstanceOf[Decoder[B]]

  final def withFieldsTransformed(f: String => String): Decoder[A] = bencode => self.apply(bencode.transformFields(f))

  final def withFieldsRenamed(f: PartialFunction[String, String]): Decoder[A] =
    bencode => self.apply(bencode.transformFields(label => f.applyOrElse(label, identity[String])))
}

object Decoder {

  def apply[A: Decoder]: Decoder[A] = implicitly

  implicit val decodeString: Decoder[String] = {
    case Bencode.BString(value) => value.decodeUtf8.left.map(err => UnexpectedValue(err.getMessage))
    case _                      => Left(UnexpectedValue("Not a string!"))
  }

  implicit val decodeLong: Decoder[Long] = {
    case Bencode.BInt(value) => Right(value)
    case _                   => Left(UnexpectedValue("Not a long!"))
  }

  implicit val decodeInt: Decoder[Int] = decodeLong.map(_.toInt)

  implicit val decodeByteVector: Decoder[ByteVector] = {
    case Bencode.BString(value) => Right(value)
    case _                      => Left(UnexpectedValue("Not a byte vector!"))
  }

  implicit val decodeByteArray: Decoder[Array[Byte]] =
    decodeByteVector.map(_.toArray)

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]] = {
    case Bencode.BList(values) => values.traverse(Decoder[A].apply)
    case _                     => Left(UnexpectedValue("Not a list!"))
  }

  implicit val decodeInstant: Decoder[Instant] =
    decodeLong.map(Instant.ofEpochSecond)

  implicit val decodeLocalDate: Decoder[LocalDate] =
    decodeInstant.map(LocalDate.ofInstant(_, ZoneId.systemDefault()))

  implicit val decodeNone: Decoder[None.type] = {
    case BEmpty => Right(None)
    case _      => Left(UnexpectedValue("It is not empty!"))
  }

  implicit def decodeSome[A: Decoder]: Decoder[Some[A]] = bencode => Decoder[A].apply(bencode).map(Some.apply)

  implicit def decodeOption[A: Decoder]: Decoder[Option[A]] =
    bencode =>
      bencode match {
        case BEmpty => Right(None)
        case other  => Decoder[A].apply(other).map(Some.apply)
      }

}
