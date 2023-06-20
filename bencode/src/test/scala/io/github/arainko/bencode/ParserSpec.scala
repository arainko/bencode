package io.github.arainko.bencode

import munit.*
import scodec.*
import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs.*
import io.github.arainko.bencode.internal.*

import scala.util.chaining.*
import scala.io.Source
import java.nio.ByteBuffer
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.immutable.SortedMap

class ParserSpec extends FunSuite {

  roundripTest("positive Bencode.Long")(
    expected = Bencode.Long(123),
    bencode = "i123e"
  )

  roundripTest("negative Bencode.Long")(
    expected = Bencode.Long(-123),
    bencode = "i-123e"
  )

  roundripTest("0 Bencode.Long")(
    expected = Bencode.Long(0),
    bencode = "i0e"
  )

  roundripTest("Bencode.String")(
    expected = Bencode.String(encode("test")),
    bencode = "4:test"
  )

  roundripTest("empty Bencode.String")(
    expected = Bencode.String(encode("")),
    bencode = "0:"
  )

  roundripTest("Bencode.Dict")(
    expected = Bencode.Dict(SortedMap(encode("key") -> Bencode.Long(1234))),
    bencode = "d3:keyi1234ee"
  )

  roundripTest("nested Bencode.Dict")(
    expected = Bencode.Dict(
      SortedMap(
        encode("nested1") -> Bencode.Dict(
          SortedMap(
            encode("nested2") -> Bencode.Dict(
              SortedMap(
                encode("nested3") -> Bencode.Long(1),
                encode("nested4") -> Bencode.Dict(SortedMap.empty)
              ),
            )
          )
        )
      )
    ),
    bencode = "d7:nested1d7:nested2d7:nested3i1e7:nested4deeee"
  )

  roundripTest("empty Bencode.Dict")(
    expected = Bencode.Dict(SortedMap.empty),
    bencode ="de"
  )

  roundripTest("Bencode.List")(
    expected = Bencode.List(Vector(Bencode.Long(1), Bencode.String(encode("string")), Bencode.List(Vector.empty))),
    bencode = "li1e6:stringlee"
  )

  roundripTest("nested Bencode.List")(
    expected = Bencode.List(Vector(Bencode.List(Vector(Bencode.Long(1))))),
    bencode = "lli1eee"
  )

  roundripTest("empty Bencode.List")(
    expected = Bencode.List(Vector.empty),
    bencode = "le"
  )

  test("torrent file"):
    println(Bencode.String(ByteVector.view("".getBytes())).byteify.decodeUtf8Lenient)
    val res = getClass().getClassLoader().getResource("ubuntu.torrent").toURI()
    val bytes = ByteVector.view(Files.readAllBytes(Path.of(res)))
    Parser.parse(bytes).toOption.get.prettyPrint.tap(println)

  private def roundripTest(description: String)(expected: Bencode, bencode: String) =
    test(s"roundrip $description"):
      val expectedBytes = encode(bencode)
      val encoded = Parser.unparse(expected)
      assertEquals(encoded, expectedBytes)
      val decoded = Parser.parse(encoded).toTry.get
      assertEquals(decoded, expected)

  private def encode(stringified: String) = ByteVector.view(stringified.getBytes())
}
