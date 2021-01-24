package rainko.bencode

import cats.instances.list._
import cats.syntax.all._

trait Decoder[A] { self =>
  def apply(bencode: Bencode): Either[String, A]

  final def map[B](f: A => B): Decoder[B] =
    bencode => self.apply(bencode).map(f)
}

object Decoder {

  def apply[A: Decoder]: Decoder[A] = implicitly

  implicit val decodeString: Decoder[String] = {
    case Bencode.BString(value) => Right(value)
    case _ => Left("Not a string!")
  }

  implicit val decodeInt: Decoder[Int] = {
    case Bencode.BInt(value) => Right(value)
    case _ => Left("Not an int!")
  }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]] = {
    case Bencode.BList(values) => values.traverse(_.decode[A])
    case _ => Left("Not a list!")
  }

  implicit def decodeMap[A: Decoder]: Decoder[Map[String, A]] = {
    case Bencode.BDict(fields) =>
      fields.values.toList
        .traverse(_.decode[A])
        .map(vals => fields.keys.map(_.value).zip(vals).toMap)
    case _ => Left("Not a dict!")
  }
}