package rainko.bencode

import rainko.bencode.Bencode._
import rainko.bencode.syntax._

trait Encoder[A] { self =>
  def apply(value: A): Bencode

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.apply(f(value))
}

object Encoder {

  trait AsObject[A] extends Encoder[A] {
    def apply(value: A): BDict
  }

  def apply[A: Encoder]: Encoder[A] = implicitly

  implicit val stringEncoder: Encoder[String] = string => BString(string.getBytes.toByteVector)

  implicit val intEncoder: Encoder[Int] = int => BInt(int.toLong)

  implicit val longEncoder: Encoder[Long] = intEncoder.contramap(_.intValue)

  implicit val booleanEncoder: Encoder[Boolean] =
    stringEncoder.contramap { bool =>
      if (bool) "true" else "false"
    }

//  implicit def encodeMap[A: Encoder]: Encoder[Map[String, A]] =
//    map =>
//      BDict {
//        map.map {
//          case (label, value) => (label, Encoder[A].apply(value))
//        }
//      }

  implicit def encodeSeq[A: Encoder]: Encoder[Seq[A]] = list => Bencode.fromSequence(list.map(Encoder[A].apply))
}
