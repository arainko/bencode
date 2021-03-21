package io.github.arainko.bencode

import scodec.bits.ByteVector

import java.nio.charset.Charset

private[bencode] object util {

  implicit class ByteArrayOps(private val byteArray: Array[Byte]) {
    def toByteVector: ByteVector                     = ByteVector.apply(byteArray)
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

}
