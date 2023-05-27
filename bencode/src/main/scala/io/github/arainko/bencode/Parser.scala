package io.github.arainko.bencode

import scodec.bits.ByteVector
import scodec.codecs.*
import scodec.codecs
import java.nio.charset.StandardCharsets

object Parser:
  def parse(bytes: ByteVector) = ???

  private val charset = StandardCharsets.UTF_8
  private val str = string(charset)
  private val digit = ???
  private def constString(value: String) = constant(ByteVector.view(value.getBytes(charset)))

  private val digits = list(str)

  private val listStart = constString("l")
  private val longStart = constString("i")
  private val dictStart = constString("d")
  private val endMarker = constString("e")

  // eg. i123e
  // private val longParser = longStart ::

