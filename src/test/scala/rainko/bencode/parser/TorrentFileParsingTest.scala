package io.github.arainko.bencode.parser

import io.github.arainko.bencode._
import io.github.arainko.bencode.derivation.semiauto._
import io.github.arainko.bencode.syntax._
import scodec.bits._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

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

  def spec: ZSpec[Environment, Failure] =
    suite("")(
      testM("should parse torrent file") {
        for {
          torrentFile <- ZStream.fromResource("ubuntu.torrent").runCollect.map(_.toArray).map(ByteVector.apply)
          parsed             = Bencode.parse(torrentFile)
          decoded            = parsed.flatMap(_.cursor.as[TorrentFile])
          info               = parsed.toOption.get.cursor.field("info").as[Info]
          parsedBackInfoHash = info.map(_.asBencode.byteify().digest("SHA-1"))
          expectedHash       = hex"0x5fff0e1c8ac414860310bcc1cb76ac28e960efbe"
        } yield assert(parsedBackInfoHash)(isRight(equalTo(expectedHash))) &&
          assert(parsed)(isRight) &&
          assert(decoded)(isRight)
      }
    )
}
