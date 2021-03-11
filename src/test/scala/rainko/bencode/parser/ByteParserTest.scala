package rainko.bencode.parser

import rainko.bencode.Bencode
import rainko.bencode.Bencode._
import rainko.bencode.syntax._
import zio.test.Assertion._
import zio.test._
import zio.test.magnolia._

import java.nio.charset.Charset

object ByteParserTest extends DefaultRunnableSpec {

  private val charsetGen = DeriveGen[StandardCharset]

  private val allCharsetGen = Gen.setOfN(6)(charsetGen)

  private val bintGen =
    Gen.anyLong.map(BInt)

  private def bstringGen(charset: Charset) =
    Gen.anyString
      .map(_.bytesWithCharset(charset).toByteVector)
      .map(BString)

  private def blistGen[R](bencodeGen: Gen[R, Bencode]) = Gen.listOf(bencodeGen).map(BList)

  private def bdictGen[R](bencodeGen: Gen[R, Bencode]) = Gen.mapOf(Gen.anyASCIIString, bencodeGen).map(BDict)

  private def bencodeGen[R](charset: Charset = Charset.defaultCharset) = {
    val atomicBencodeGen = Gen.oneOf(bintGen, bstringGen(charset))
    val bencodeGen       = Gen.oneOf(atomicBencodeGen, blistGen(atomicBencodeGen), bdictGen(atomicBencodeGen))
    val listGen = blistGen(bencodeGen)
    val dictGen = bdictGen(Gen.oneOf(bencodeGen, atomicBencodeGen, listGen))
    Gen.oneOf(atomicBencodeGen, bencodeGen, listGen, dictGen)
  }

  private val defaultCharsetParserSuite = suite("Byte Parser parsers should")(
    testM("parse BInt") {
      check(bintGen) { int =>
        val stringified = int.stringify
          .bytesWithCharset(Charset.defaultCharset)
          .toByteVector
        val parsed = Bencode.parse(stringified)
        assert(parsed)(isRight(equalTo(int)))
      }
    },
    testM("parse BString") {
      check(bstringGen(Charset.defaultCharset)) { bstring =>
        val sringified = bstring.stringify
          .bytesWithCharset(Charset.defaultCharset)
          .toByteVector
        val parsed = Bencode.parse(sringified)
        assert(parsed)(isRight(equalTo(bstring)))
      }
    },
    testM("parse BList") {
      check(blistGen(bencodeGen())) { blist =>
        val sringified = blist.stringify
          .bytesWithCharset(Charset.defaultCharset)
          .toByteVector
        val parsed = Bencode.parse(sringified)
        assert(parsed)(isRight(equalTo(blist)))
      }
    },
    testM("parse BDict") {
      check(bdictGen(bencodeGen())) { bdict =>
        val sringified = bdict.stringify
          .bytesWithCharset(Charset.defaultCharset)
          .toByteVector
        val parsed = Bencode.parse(sringified)
        assert(parsed)(isRight(equalTo(bdict)))
      }
    }
  )

  def spec: ZSpec[Environment, Failure] =
    suite("Byte Parser suite")(
      defaultCharsetParserSuite
    )

}
