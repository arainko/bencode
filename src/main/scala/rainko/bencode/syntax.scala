package rainko
package rainko.bencode

object syntax {
  implicit class BencodeOps[A](val value: A) extends AnyVal {
    def encode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
