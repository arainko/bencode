package io.github.arainko.bencode

import scodec.bits.ByteVector
import scodec.codecs.*
import scodec.codecs
import java.nio.charset.StandardCharsets
import scodec.Codec
import scala.collection.immutable.SortedMap
import scodec.Attempt
import scodec.bits.BitVector
import scodec.SizeBound
import scodec.Err
import scodec.DecodeResult

final class Utf8DigitCodec(digit: Char) extends Codec[Char]:

  private val digitBits = ByteVector.view(digit.toString.getBytes).bits

  override def sizeBound = SizeBound.exact(digitBits.size)

  override def encode(that: Char) =
    Attempt.guard(that == digit, s"expected digit '$digit' but got '$that'").map(_ => digitBits)

  override def decode(buffer: BitVector) =
    buffer.acquire(digitBits.size) match
      case Left(_) => Attempt.failure(Err.insufficientBits(digitBits.size, buffer.size))
      case Right(b) =>
        if b == digitBits then Attempt.successful(DecodeResult(digit, buffer.drop(digitBits.size)))
        else Attempt.failure(Err(s"expected constant digit $digit with bits $digitBits but got ${String(b.toByteArray)}"))

  override def toString = s"digit($digit)"

private[bencode] object Parser:
  def decode(bytes: ByteVector) = bencode.decode(bytes.bits)

  def encode(value: Bencode) = bencode.encode(value).map(_.bytes)

  private val charset = StandardCharsets.UTF_8

  private def constString(value: String) = constant(ByteVector.view(value.getBytes(charset))).withContext("constant")

  private val listStart = constString("l")
  val longStart = constString("i")
  private val dictStart = constString("d")
  private val endMarker = constString("e")

  private def bytes(size: Long) = fixedSizeBytes(size, bits).xmapc(_.bytes)(_.bits)

  val digit = codecs.choice(('0' to '9').map(Utf8DigitCodec(_))*)

  val digits =
    list(digit)
      .xmap(_.mkString.toLong, a => a.toString.toList)

  val long = (longStart ~> digits.xmap[Bencode.Long](Bencode.Long.apply, _.value) <~ endMarker).withContext("Bencode.Long")

  val string =
    (digits <~ constString(":"))
      .flatZip(bytes)
      .xmap[Bencode.String]((_, bytes) => Bencode.String(bytes), str => str.value.size -> str.value)
      .withContext("Bencode.String")

  val bencode: Codec[Bencode] =
    Codec.lazily[Bencode]:
      val list = (listStart ~>
        codecs
          .list(bencode)
          .xmap[Bencode.List](l => Bencode.List(l.toVector), _.values.toList) <~ endMarker)
        .withContext("Bencode.List")

      val dict = (dictStart ~> codecs
        .list(string :: bencode)
        .xmap[Bencode.Dict](
          pairs => Bencode.Dict(SortedMap.from(pairs.map((str, bencode) => String(str.value.toArray) -> bencode))),
          dict =>
            dict.values
              .map((str, bencode) => Bencode.String(ByteVector.view(str.getBytes)) -> bencode)(Ordering.by(_.value))
              .toList
        ) <~ endMarker).withContext("Bencode.Dict")

      codecs.choice[Bencode](string.upcast, long.upcast, list.upcast, dict.upcast)
  end bencode

  // private val bencode = Codec.lazily[Bencode]()

  // eg. i123e
  // private val longParser = longStart ::
