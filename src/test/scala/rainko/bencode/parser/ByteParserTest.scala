package io.github.arainko.bencode.parser

import io.github.arainko.bencode.Bencode
import io.github.arainko.bencode.Bencode._
import io.github.arainko.bencode.util._
import zio.test.Assertion._
import zio.test._

import java.nio.charset.{Charset, StandardCharsets}

object ByteParserTest extends DefaultRunnableSpec {

  private val charsetsTestedAgainst =
    StandardCharsets.ISO_8859_1 ::
      StandardCharsets.US_ASCII ::
      StandardCharsets.UTF_8 ::
      Nil

  private val bintGen =
    Gen.anyLong.map(BInt)

  private def bstringGen =
    Gen
      .chunkOf(Gen.anyByte)
      .map(_.toArray.toByteVector)
      .map(BString)

  private def blistGen[R](bencodeGen: Gen[R, Bencode]) = Gen.listOf(bencodeGen).map(BList)

  private def bdictGen[R](bencodeGen: Gen[R, Bencode]) = Gen.mapOf(Gen.anyASCIIString, bencodeGen).map(Bencode.fromMap)

  private def bencodeGen[R] = {
    val atomicBencodeGen = Gen.oneOf(bintGen, bstringGen)
    val bencodeGen       = Gen.oneOf(atomicBencodeGen, blistGen(atomicBencodeGen), bdictGen(atomicBencodeGen))
    val listGen          = blistGen(bencodeGen)
    val dictGen          = bdictGen(Gen.oneOf(bencodeGen, atomicBencodeGen, listGen))
    Gen.oneOf(atomicBencodeGen, bencodeGen, listGen, dictGen)
  }

  private def charsetParserTests(charset: Charset) =
    List(
      testM(s"parse BInt ($charset)") {
        check(bintGen) { int =>
          val stringified = int.byteify(charset)
          val parsed      = Bencode.parse(stringified, charset)
          assert(parsed)(isRight(equalTo(int)))
        }
      },
      testM(s"parse BString ($charset)") {
        check(bstringGen) { bstring =>
          val sringified = bstring.byteify(charset)
          val parsed     = Bencode.parse(sringified, charset)
          assert(parsed)(isRight(equalTo(bstring)))
        }
      },
      testM(s"parse BList ($charset)") {
        check(blistGen(bencodeGen)) { blist =>
          val sringified = blist.byteify(charset)
          val parsed     = Bencode.parse(sringified, charset)
          assert(parsed)(isRight(equalTo(blist)))
        }
      },
      testM(s"parse BDict ($charset)") {
        check(bdictGen(bencodeGen)) { bdict =>
          val sringified = bdict.byteify(charset)
          val parsed     = Bencode.parse(sringified, charset)
          assert(parsed)(isRight(equalTo(bdict)))
        }
      }
    )

  def spec: ZSpec[Environment, Failure] =
    suite("Byte Parser should")(
      charsetsTestedAgainst.flatMap(charsetParserTests): _*
    )

}
