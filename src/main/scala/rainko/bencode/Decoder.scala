package rainko.bencode

import cats.instances.list._
import cats.syntax.all._
import rainko.bencode.Bencode.BDict
import rainko.bencode.BencodeError._
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

  final def or(other: Decoder[A]): Decoder[A] = bencode => self.apply(bencode).orElse(other.apply(bencode))

  final def withFieldsTransformed(f: String => String): Decoder[A] = {
    case bdict @ BDict(_) => self.apply(Decoder.deepTransformFields(bdict, f))
    case _                => Left(UnexpectedValue("Fields cannot be renamed as the bencode isn't a BDict!"))
  }

  final def withFieldsRenamed(f: PartialFunction[String, String]): Decoder[A] = {
    case bdict @ BDict(_) => self.apply(Decoder.deepRenameFields(bdict, f))
    case _                => Left(UnexpectedValue("Fields cannot be renamed as the bencode isn't a BDict!"))
  }
}

object Decoder {

  trait AsObject[A] extends Decoder[A] { self =>
    def decodeAsObject(value: BDict): Either[DecodingError, A]

    final override def apply(bencode: Bencode): Either[DecodingError, A] =
      bencode match {
        case dict @ BDict(_) => decodeAsObject(dict)
        case _               => Left(UnexpectedValue("Not a dict!"))
      }
  }

  def apply[A: Decoder]: Decoder[A] = implicitly

  implicit def decodeOption[A: Decoder]: Decoder[Option[A]] =
    bencode => {
      println("decodeding option")
      Decoder[A].apply(bencode) match {
        case Left(value) => Right(None)
        case Right(value) => Right(Some(value))
      }
    }

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

  private def deepTransformFields(bdict: BDict, f: String => String): BDict = {
    val deepTransformed = bdict.fields.map { case (field, value) =>
      f(field) -> (value match {
        case deepBDict @ BDict(_) => deepTransformFields(deepBDict, f)
        case bencode              => bencode
      })
    }
    BDict(deepTransformed)
  }

  private def deepRenameFields(bdict: BDict, f: PartialFunction[String, String]): BDict = {
    val deepTransformed = bdict.fields.map { case (field, value) =>
      f.applyOrElse(field, identity[String]) -> (value match {
        case deepBDict @ BDict(_) => deepRenameFields(deepBDict, f)
        case bencode              => bencode
      })
    }
    BDict(deepTransformed)
  }
}
