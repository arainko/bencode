package io.github.arainko.bencode.internal

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
import scala.util.chaining.*
import io.github.arainko.bencode.*

final class RepeatUntil[A](codec: Codec[A], endMarker: Byte) extends Codec[List[A]] {
  private val endMarkerBytes = ByteVector(endMarker)
  private val listOfA = list(codec)

  override def sizeBound: SizeBound = SizeBound.atLeast(8)

  override def encode(value: List[A]): Attempt[BitVector] =
    listOfA.encode(value).map(_ ++ endMarkerBytes.bits)

  override def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] =
    def loop(acc: List[A], bits: BitVector): Attempt[DecodeResult[List[A]]] =
      codec
        .decode(bits)
        .flatMap: res =>
          if res.remainder.bytes.startsWith(endMarkerBytes) then
            Attempt.successful(DecodeResult(res.value :: acc, res.remainder.drop(8)))
          else loop(res.value :: acc, res.remainder)

    loop(Nil, bits).map(_.map(_.reverse))
}

private[bencode] object Parser:
  def parse(bytes: ByteVector) = bencode.complete.decode(bytes.bits).map(_.value)

  def unparse(value: Bencode) = bencode.encode(value).map(_.bytes).require

  private val Utf8 = StandardCharsets.UTF_8

  private def constString(value: String) = constant(ByteVector.view(value.getBytes(Utf8))).withContext("constant")

  private val listStart = constString("l")
  private val longStart = constString("i")
  private val dictStart = constString("d")
  private val `e` = 0x65.toByte // 'e' in utf-8
  private val `:` = 0x3a.toByte // ':' in utf-8

  private def bytes(size: Long) = fixedSizeBytes(size, bits).xmapc(_.bytes)(_.bits)

  val digit =
    val digits = "0123456789".getBytes(Utf8)
    val initial = scodec.codecs.discriminated[Byte].by(byte)
    digits.foldLeft(initial)((codec, digitByte) => codec.singleton(digitByte, digitByte))
  end digit

  def number(endMarker: Byte) =
    val positive =
      RepeatUntil(digit, endMarker).xmap(
        bytes => String(bytes.toArray, Utf8).toLong,
        long => long.toString.getBytes(Utf8).toList
      )
    val negative = constString("-") ~> positive.xmap(long => -long, long => -long)
    choice(negative, positive)
  end number

  val long = (longStart ~> number(e).xmap[Bencode.Long](Bencode.Long.apply, _.value)).withContext("Bencode.Long")

  val string =
    number(`:`)
      .flatZip(bytes)
      .xmap[Bencode.String]((_, bytes) => Bencode.String(bytes), str => str.value.size -> str.value)
      .withContext("Bencode.String")

  val bencode: Codec[Bencode] =
    Codec.lazily[Bencode]:
      val list = (listStart ~>
        RepeatUntil(bencode, e)
          .xmap[Bencode.List](l => Bencode.List(l.toVector), _.values.toList))
        .withContext("Bencode.List")

      val dict = (dictStart ~>
        RepeatUntil(string :: bencode, e)
          .xmap[Bencode.Dict](
            pairs => Bencode.Dict(SortedMap.from(pairs.map((str, bencode) => str.value -> bencode))),
            _.values.toList.map((str, bencode) => Bencode.String(str) -> bencode)
          )).withContext("Bencode.Dict")

      codecs.choice[Bencode](string.upcast, long.upcast, list.upcast, dict.upcast)
  end bencode

  // private val bencode = Codec.lazily[Bencode]()

  // eg. i123e
  // private val longParser = longStart ::
