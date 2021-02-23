package rainko.bencode

import cats.instances.list._
import cats.syntax.all._
import rainko.bencode.syntax._

import java.time.{Instant, LocalDate, ZoneId}

trait Decoder[A] { self =>
  def apply(bencode: Bencode): Either[String, A]

  final def map[B](f: A => B): Decoder[B] =
    bencode => self.apply(bencode).map(f)
}

object Decoder {

  def apply[A: Decoder]: Decoder[A] = implicitly

  implicit val decodeString: Decoder[String] = {
    case Bencode.BString(value) => Right(value.toArray.utf8String)
    case _ => Left("Not a string!")
  }

  implicit val decodeInt: Decoder[Int] = {
    case Bencode.BInt(value) => Right(value)
    case _ => Left("Not an int!")
  }

  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]] = {
    case Bencode.BList(values) => values.traverse(Decoder[A].apply)
    case _ => Left("Not a list!")
  }

  implicit val decodeInstant: Decoder[Instant] =
    decodeInt.map(_.toLong).map(Instant.ofEpochSecond)

  implicit val decodeLocalDate: Decoder[LocalDate] =
    decodeInstant.map(LocalDate.ofInstant(_, ZoneId.systemDefault()))

}