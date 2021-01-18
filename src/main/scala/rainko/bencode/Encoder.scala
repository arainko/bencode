package rainko
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

//  implicit def encodeMap[A: Encoder]: Encoder[Map[String, A]] =
//    map =>
//      BDict {
//        map.map {
//          case (label, value) => (label, Encoder[A].apply(value))
//        }
//      }

  implicit def encodeList[A: Encoder]: Encoder[List[A]] =
    list => BList(list.map(Encoder[A].apply): _*)
}
