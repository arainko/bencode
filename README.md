## Bencode for Scala

Typesafe bencode encoders and decoders with semi-automatic derivation.

### Installation

All releases are published on [Maven Central](https://search.maven.org/artifact/io.github.arainko/bencode_2.13).

To use the library add this to your `build.sbt`:
```scala

libraryDependencies += "io.github.arainko" %% "bencode" % "0.1.0"

```

### Example

Assuming these imports:

``` scala
import scodec.bits._
import zio.stream.ZStream
import io.github.arainko.bencode._
import io.github.arainko.bencode.syntax._
import io.github.arainko.bencode.derivation.semiauto._
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

``` scala
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

We can parse the .torrent file almost boilerplate free (always make sure you derive the 'inner' parts of your case class/ADT first!):

```  scala
  implicit val infoDecoder: Decoder[Info] =
    deriveDecoder[Info]
      .withFieldsRenamed { case "piece length" =>"pieceLength" }

  implicit val decoder: Decoder[TorrentFile] = deriveDecoder[TorrentFile].withFieldsRenamed {
    case "created by"    => "createdBy"
    case "creation date" => "creationDate"
    case "announce-list" => "announceList"
  }
 
 val decoded = 
  for {
    torrentFile <- ZStream.fromResource("ubuntu.torrent").runCollect.map(_.toArray).map(ByteVector.apply)
    parsed  = Bencode.parse(torrentFile)
    decoded = parsed.flatMap(_.cursor.as[TorrentFile])
  } yield decoded
 
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

### Acknowledgements
This library's design is heavily influenced by the great [circe](https://circe.github.io/circe/),
uses the just-as-great [shapeless](https://github.com/milessabin/shapeless) for typeclass derivation
and makes use of [scodec's](https://github.com/scodec/scodec) `ByteVector` for binary representation 
of bencoded values.
