package io.github.arainko.bencode

object syntax {

  implicit class EncoderOps[A](private val value: A) extends AnyVal {
    def asBencode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
