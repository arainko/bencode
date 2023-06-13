package io.github.arainko.bencode

import munit.*
import scodec.DecodeResult
import scodec.bits.ByteVector

import scala.util.chaining.*
import scodec.bits.BitVector

class ParserSpec extends FunSuite {
  roundripTest("Bencode.Long")(
    expected = Bencode.Long(1),
    bencode = "i1e"
  )

  roundripTest("Bencode.String")(
    expected = Bencode.String(encode("test")),
    bencode = "4:test"
  )

  test("dupal") {
    Parser.digits.decode(BitVector("12317283e".getBytes)).tap(println)
  }

  private def roundripTest(description: String)(expected: Bencode, bencode: String) =
    test(s"roundrip $description"):
      val expectedBytes = encode(bencode)
      val encoded = Parser.encode(expected).getOrElse(throw new RuntimeException("encoding failed :("))
      assertEquals(encoded, expectedBytes)
      val decoded = Parser.decode(encoded).toTry.get
      assert(decoded.remainder.isEmpty)
      assertEquals(decoded.value, expected)

  private def encode(stringified: String) = ByteVector.view(stringified.getBytes())
}
