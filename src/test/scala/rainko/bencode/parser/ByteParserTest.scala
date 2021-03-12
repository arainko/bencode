package rainko.bencode.parser

import rainko.bencode.Bencode
import rainko.bencode.Bencode._
import rainko.bencode.syntax._
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

  private def bstringGen(charset: Charset) =
    Gen
      .chunkOf(Gen.anyByte)
      .map(_.toArray.toByteVector)
      .map(BString)

  private def blistGen[R](bencodeGen: Gen[R, Bencode]) = Gen.listOf(bencodeGen).map(BList)

  private def bdictGen[R](bencodeGen: Gen[R, Bencode]) = Gen.mapOf(Gen.anyASCIIString, bencodeGen).map(BDict)

  private def bencodeGen[R](charset: Charset) = {
    val atomicBencodeGen = Gen.oneOf(bintGen, bstringGen(charset))
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
        check(bstringGen(charset)) { bstring =>
          val sringified = bstring.byteify(charset)
          val parsed     = Bencode.parse(sringified, charset)
          assert(parsed)(isRight(equalTo(bstring)))
        }
      },
      testM(s"parse BList ($charset)") {
        check(blistGen(bencodeGen(charset))) { blist =>
          val sringified = blist.byteify(charset)
          val parsed     = Bencode.parse(sringified, charset)
          assert(parsed)(isRight(equalTo(blist)))
        }
      },
      testM(s"parse BDict ($charset)") {
        check(bdictGen(bencodeGen(charset))) { bdict =>
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
