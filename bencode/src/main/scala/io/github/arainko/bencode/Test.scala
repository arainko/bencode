package io.github.arainko.bencode

import cats.syntax.all.*
import io.github.arainko.bencode.internal.Parser
import scodec.bits.{ ByteVector }

import scala.util.chaining.*

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
  val encoded = Test.codec.encode(inst).tap(println)
  println(Parser.unparse(encoded).tap(a => println(a.decodeUtf8)).pipe(Parser.parse))
  println(Test.codec.decode(encoded))
