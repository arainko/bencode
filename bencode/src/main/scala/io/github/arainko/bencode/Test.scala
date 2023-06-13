package io.github.arainko.bencode

import cats.syntax.all.*
import scodec.bits.ByteVector
import scodec.bits.BitVector

final case class Test(int: Long, str: String, opt: Option[Long], cos: Vector[Long])

object Test {
  given codec: Codec[Test] = 
    Codec.product: field =>
      (
        field("int", _.int),
        field("str", _.str),
        field.opt("opt", _.opt),
        field("cos", _.cos)
      ).mapN(Test.apply)
}

@main def main = 
  val inst = Test(20L, "asd", None, Vector(1, 2, 3, 4))
  val encoded = Test.codec.encode(inst)
  println(Parser.encode(encoded).map(Parser.decode))
  println('5'.toString())
  println(Test.codec.decode(encoded))
