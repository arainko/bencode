package rainko.bencode

import zio.test._
import zio.test.Assertion._
import rainko.bencode.syntax._
import java.nio.file.Files
import java.nio.file.Paths
import rainko.bencode.derivation.auto._
import scodec.bits.ByteVector

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

  private val torrentFilePath = Paths.get(
    "/home/aleksander/IdeaProjects/bencode/src/test/resources/ubuntu.torrent"
  )
  private val torrentFile = Files.readAllBytes(torrentFilePath).toByteVector

  def spec: ZSpec[Environment, Failure] =
    test("should parse torrent file") {
      val parsed = Bencode.parse(torrentFile)

      implicit val decoder = Decoder[TorrentFile].withFieldsRenamed {
        case "created by"    => "createdBy"
        case "creation date" => "creationDate"
        case "announce-list" => "announceList"
        case "piece length"  => "pieceLength"
      }

      val decoded = parsed.flatMap(_.cursor.as[TorrentFile])
      assert(parsed)(isRight)
    }
}
