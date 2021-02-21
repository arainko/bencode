package rainko.bencode

import java.nio.file.{Files, Paths}
import rainko.bencode.derivation.decoder._
import rainko.bencode.derivation.encoder._
import syntax._

final case class Info(length: Int, name: String, `piece length`: Int)
final case class TorrentFile(
    info: Info,
    announce: String,
    comment: String,
    `creation date`: Int,
    httpseeds: Seq[String]
)

object Main extends App {

  // val file: Array[Byte] = Files.readAllBytes(
  //   Paths.get(
  //     "/home/aleksander/IdeaProjects/bencode/src/main/resources/debian.torrent"
  //   )
  // )

  // println(file.drop(538).toSeq.size)

  // val cos = new String(file).length
  
  // println(cos)

  // println(cos.dropRight(10))

  val torrentFile =
    "d8:announce41:http://bttracker.debian.org:6969/announce7:comment35:\"Debian CD from cdimage.debian.org\"13:creation datei1573903810e9:httpseedsl145:https://cdimage.debian.org/cdimage/release/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.iso145:https://cdimage.debian.org/cdimage/archive/10.2.0//srv/cdbuilder.debian.org/dst/deb-cd/weekly-builds/amd64/iso-cd/debian-10.2.0-amd64-netinst.isoe4:infod6:lengthi351272960e4:name31:debian-10.2.0-amd64-netinst.iso12:piece lengthi262144eee"
  val parsed = Bencode.parse(torrentFile).flatMap(_.cursor.as[TorrentFile])
  val encoded = parsed.map(_.encode.prettyStringify)
  println(encoded)
}

