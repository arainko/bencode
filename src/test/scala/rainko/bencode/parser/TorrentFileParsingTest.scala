package rainko.bencode.parser

import rainko.bencode._
import rainko.bencode.derivation.semiauto._
import rainko.bencode.syntax._
import scodec.bits.ByteVector
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

  implicit val infoDecoder: Decoder[Info] = deriveDecoder[Info]

  implicit val decoder: Decoder[TorrentFile] = deriveDecoder[TorrentFile].withFieldsRenamed {
    case "created by"    => "createdBy"
    case "creation date" => "creationDate"
    case "announce-list" => "announceList"
    case "piece length"  => "pieceLength"
  }

  private val torrentFilePath = Paths.get(
    "/home/aleksander/IdeaProjects/bencode/src/test/resources/ubuntu.torrent"
  )
  private val torrentFile = Files.readAllBytes(torrentFilePath).toByteVector

  def spec: ZSpec[Environment, Failure] =
    test("should parse torrent file") {
      val parsed  = Bencode.parse(torrentFile)
      val decoded = parsed.flatMap(_.cursor.as[TorrentFile])
      assert(parsed)(isRight) &&
      assert(decoded)(isRight)
    }
}
