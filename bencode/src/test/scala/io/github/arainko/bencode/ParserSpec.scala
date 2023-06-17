package io.github.arainko.bencode

import munit.*
import scodec.*
import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs.*
import io.github.arainko.bencode.internal.*

import scala.util.chaining.*

class ParserSpec extends FunSuite {

  test("test") {
    import scodec.codecs.*

    Parser.number(0x3a).decode(encode("-123:").bits).tap(println)
    // Parser.number(0x3a).encode(-123).tap(println)
    logToStdOut(Parser.number(0x3a), "number codec").encode(-123)

    // delim.encode(List(1, 2, 3, 5)).map(_.decodeUtf8).tap(println)

    // digit.encode(3).map.tap(println)

  }

  roundripTest("positive Bencode.Long")(
    expected = Bencode.Long(123),
    bencode = "i123e"
  )

  roundripTest("negative Bencode.Long")(
    expected = Bencode.Long(-123),
    bencode = "i-123e"
  )

  roundripTest("Bencode.String")(
    expected = Bencode.String(encode("test")),
    bencode = "4:test"
  )

  private def roundripTest(description: String)(expected: Bencode, bencode: String) =
    test(s"roundrip $description"):
      val expectedBytes = encode(bencode)
      val encoded = Parser.encode(expected).toTry.get
      assertEquals(encoded, expectedBytes)
      val decoded = Parser.decode(encoded).toTry.get
      assert(decoded.remainder.isEmpty)
      assertEquals(decoded.value, expected)

  private def encode(stringified: String) = ByteVector.view(stringified.getBytes())
}
