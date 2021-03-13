package rainko.bencode.parser

import rainko.bencode._
import rainko.bencode.derivation.semiauto._
import rainko.bencode.syntax._
import scodec.bits._
import zio.test.Assertion._
import zio.test._

import java.nio.file.{Files, Paths}

object TorrentFileParsingTest extends DefaultRunnableSpec {

  final case class Info(length: Long, name: String, pieceLength: Long, pieces: ByteVector)

  final case class TorrentFile(
    info: Info,
    announce: String,
    comment: String,
    createdBy: String,
    creationDate: Long,
    announceList: Seq[Seq[String]]
  )

  implicit val infoDecoder: Decoder[Info] =
    deriveDecoder[Info]
      .withFieldsRenamed { case "piece length" =>
        "pieceLength"
      }

  implicit val infoEncoder: Encoder[Info] =
    deriveEncoder[Info]
      .withFieldsRenamed { case "pieceLength" => "piece length" }

  implicit val decoder: Decoder[TorrentFile] = deriveDecoder[TorrentFile].withFieldsRenamed {
    case "created by"    => "createdBy"
    case "creation date" => "creationDate"
    case "announce-list" => "announceList"
  }

  private val torrentFilePath = Paths.get(
    "/home/aleksander/IdeaProjects/bencode/src/test/resources/ubuntu.torrent"
  )
  private val torrentFile = Files.readAllBytes(torrentFilePath).toByteVector

  def spec: ZSpec[Environment, Failure] =
    suite("")(
      test("should parse torrent file") {
        val parsed  = Bencode.parse(torrentFile)
        val decoded = parsed.flatMap(_.cursor.as[TorrentFile])
        val info    = parsed.toOption.get.cursor.field("info").as[Info]
        val parsedBackInfoHash = info.map(_.encode.byteify().digest("SHA-1"))
        val expectedHash = hex"0x5fff0e1c8ac414860310bcc1cb76ac28e960efbe"
        assert(parsedBackInfoHash)(isRight(equalTo(expectedHash))) &&
        assert(parsed)(isRight) &&
        assert(decoded)(isRight)
      }
    )
}
