## Bencode for Scala

Typesafe bencode encoder/decoder with automatic derivation.

### Example

Assuming these imports:

```
import java.nio.file.Files
import java.nio.file.Paths
import scodec.bits.ByteVector
import rainko.bencode.syntax._
import rainko.bencode.derivation.auto._
import rainko.bencode.parser.ByteParser
```

Given a torrent file (eg. `https://releases.ubuntu.com/20.10/ubuntu-20.10-desktop-amd64.iso.torrent`) with a structure like this:

```
d
8:announce  35:https://torrent.ubuntu.com/announce
13:creation date  i1603385085e
13:announce-list
  l
    l
    35:https://torrent.ubuntu.com/announce
    e
    l
    40:https://ipv6.torrent.ubuntu.com/announce
    e
  e
10:created by  13:mktorrent 1.1
4:info
  d
  6:length  i2942003200e
  4:name  30:ubuntu-20.10-desktop-amd64.iso
  12:piece length  i262144e
  6:pieces  224460: <blob>
  e
7:comment  29:Ubuntu CD releases.ubuntu.com
e
```

and a structure of case classes:

```
  final case class Info(length: Long, name: String, pieceLength: Long, pieces: ByteVector)

  final case class TorrentFile(
    info: Info,
    announce: String,
    comment: String,
    createdBy: String,
    creationDate: Long,
    announceList: Seq[Seq[String]]
  )
```

We can parse the .torrent file almost boilerplate free:

```
 private val torrentFilePath = Paths.get(
    "/home/aleksander/IdeaProjects/bencode/src/test/resources/ubuntu.torrent"
  )
  
 private val torrentFile = Files.readAllBytes(torrentFilePath).toByteVector
  
 val parsed = ByteParser.parse(torrentFile)

 implicit val decoder = Decoder[TorrentFile].withFieldsRenamed {
   case "created by"    => "createdBy"
   case "creation date" => "creationDate"
   case "announce-list" => "announceList"
   case "piece length"  => "pieceLength"
 }
 
 val decoded = parsed.flatMap(_.cursor.as[TorrentFile])
 
 // output
 Right(
   TorrentFile(
      info = Info(
        length = 2942003200,
        name = ubuntu-20.10-desktop-amd64.iso,262144,
        pieces = ByteVector(224460 bytes, #-469656818)
      ),
      announce = https://torrent.ubuntu.com/announce,
      name = Ubuntu CD releases.ubuntu.com,
      createdBy = mktorrent 1.1,
      creationDate = 1603385085,
      announceList = List(List(https://torrent.ubuntu.com/announce), List(https://ipv6.torrent.ubuntu.com/announce))
   )
 )
```

### Docs
TODO
