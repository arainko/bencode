package rainko.bencode


object syntax {
  implicit class EncoderOps[A](private val value: A) extends AnyVal {
    def encode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
