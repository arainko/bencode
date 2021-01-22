package rainko.bencode

trait Decoder[A] {
  def apply(bencode: Bencode): Either[String, A]
}

object Decoder {
//  implicit val decodeString
}