package rainko.bencode

import cats.instances.list._
import cats.syntax.all._
import rainko.bencode.Bencode.BDict
import scodec.bits.ByteVector

import java.time.{Instant, LocalDate, ZoneId}

trait Decoder[A] { self =>
  def apply(bencode: Bencode): Either[String, A]

  final def map[B](f: A => B): Decoder[B] = bencode => self.apply(bencode).map(f)

  final def withFieldsTransformed(f: String => String): Decoder[A] = {
    case bdict @ BDict(_) => self.apply(Decoder.deepTransformFields(bdict, f))
    case _                => Left("Fields cannot be renamed as the bencode isn't a BDict!")
  }

  final def withFieldsRenamed(f: PartialFunction[String, String]): Decoder[A] = {
    case bdict @ BDict(_) => self.apply(Decoder.deepRenameFields(bdict, f))
    case _                => Left("Fields cannot be renamed as the bencode isn't a BDict!")
  }
}

object Decoder {

  trait AsObject[A] extends Decoder[A] { self =>
    def decodeAsObject(value: BDict): Either[String, A]

    final override def apply(bencode: Bencode): Either[String, A] =
      bencode match {
        case dict @ BDict(_) => decodeAsObject(dict)
        case _               => Left("Not a dict!")
      }
  }

  def apply[A: Decoder]: Decoder[A] = implicitly

  implicit val decodeString: Decoder[String] = {
    case Bencode.BString(value) => value.decodeUtf8.left.map(_.getMessage)
    case _                      => Left("Not a string!")
  }

  implicit val decodeLong: Decoder[Long] = {
    case Bencode.BInt(value) => Right(value)
    case _                   => Left("Not a long!")
  }

  implicit val decodeInt: Decoder[Int] = decodeLong.map(_.toInt)

  implicit val decodeByteVector: Decoder[ByteVector] = {
    case Bencode.BString(value) => Right(value)
    case _                      => Left("Not a byte vector!")
  }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]] = {
    case Bencode.BList(values) => values.traverse(Decoder[A].apply)
    case _                     => Left("Not a list!")
  }

  implicit val decodeInstant: Decoder[Instant] =
    decodeInt.map(_.toLong).map(Instant.ofEpochSecond)

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
