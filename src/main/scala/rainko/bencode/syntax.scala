package rainko.bencode

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets


object syntax {
  implicit class ByteArrayOps(private val byteArray: Array[Byte]) {
    def utf8String: String = new String(byteArray, StandardCharsets.UTF_8)
  }

  implicit class EncoderOps[A](private val value: A) extends AnyVal {
    def encode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
