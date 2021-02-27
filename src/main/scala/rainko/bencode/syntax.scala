package rainko.bencode

import scodec.bits.ByteVector
import java.nio.charset.Charset

object syntax {

  implicit class ByteArrayOps(private val byteArray: Array[Byte]) {
    def toByteVector: ByteVector = ByteVector.apply(byteArray)
    def stringUsingCharset(charset: Charset): String = new String(byteArray, charset)
  }

  implicit class StringOps(private val string: String) {
    def bytesWithCharset(implicit charset: Charset): Array[Byte] = string.getBytes(charset)
  }

  implicit class ByteVectorOps(private val byteVector: ByteVector) {

    def parseWithCharset(implicit charset: Charset): Either[BencodeError.ParsingFailure, String] =
      byteVector.decodeString.left
        .map(err => BencodeError.ParsingFailure(err.getMessage, s"${byteVector.take(10).toString}..."))
  }

  implicit class EncoderOps[A](private val value: A) extends AnyVal {
    def encode(implicit enc: Encoder[A]): Bencode = enc(value)
  }
}
