package rainko.bencode.parser

import zio.test._
import zio.test.Assertion._
import rainko.bencode.syntax._
import java.nio.charset.StandardCharsets
import rainko.bencode.Bencode

object ByteParserTest extends DefaultRunnableSpec {

  private val bintByteGen    = Gen.anyInt.map(int => s"i${int}e").map(_.getBytes.toByteVector)
  private val bstringByteGen = Gen.anyString.map(string => s"${string.length}:$string").map(_.getBytes.toByteVector)

  private val parserSuite = suite("Byte Parser parsers should")(
    testM("parse BInt") {
      check(bintByteGen) { bint =>
        val expectedParsed = bint
          .parseWithCharset(StandardCharsets.UTF_8)
          .map(string => string.drop(1).dropRight(1).toLong)
          .map(Bencode.BInt)
        val parsed = ByteParser.parseBInt(bint)
        assert(parsed)(equalTo(expectedParsed))
      }
    },
    testM("parse BString") {
      check(bstringByteGen) { bstring =>
        val parsed = ByteParser.parseBString(bstring)
        assert(parsed)(isRight)
      }
    }
  )

  private val matcherSuite = suite("Byte Parser matchers should")(
    testM("match BInt") {
      check(bintByteGen) { bint =>
        val matchResult = ByteParser.matchBInt(bint)
        assert(matchResult)(isTrue)
      }
    },
    testM("match BString") {
      check(bstringByteGen) { bstring =>
        val matchResult = ByteParser.matchBString(bstring)
        assert(matchResult)(isTrue)
      }
    }
    // test("match BInt") {
    //   val bint = "i12345e".getBytes.toByteVector
    //   val matchResult = ByteParser.matchBInt(bint)
    //   assert(matchResult)(isTrue)
    // },
    // test("match ")
  )

  def spec: ZSpec[Environment, Failure] =
    suite("Byte Parser suite")(
      matcherSuite,
      parserSuite
    )

}
