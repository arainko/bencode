package rainko.bencode

import Bencode._

trait Encoder[A] { self =>
  def apply(value: A): Bencode

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.apply(f(value))
}

object Encoder {
  def apply[A: Encoder]: Encoder[A] = implicitly

  implicit val stringEncoder: Encoder[String] = BString(_)

  implicit val intEncoder: Encoder[Int] = BInt(_)

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

  implicit def encodeSeq[A: Encoder]: Encoder[Seq[A]] =
    list => BList(list.map(Encoder[A].apply): _*)
}
