package rainko.bencode

import java.nio.charset.StandardCharsets
import scodec.bits.ByteVector


object syntax {
  implicit class ByteArrayOps(private val byteArray: Array[Byte]) {
    def toByteVector: ByteVector = ByteVector.apply(byteArray)
    def utf8String: String = new String(byteArray, StandardCharsets.UTF_8)
  }

  implicit class EncoderOps[A](private val value: A) extends AnyVal {
    def encode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
