package rainko.bencode

import rainko.bencode.Bencode._
import scodec.bits.ByteVector

trait Encoder[A] { self =>
  def apply(value: A): Bencode

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.apply(f(value))

  final def withFieldsRenamed(f: PartialFunction[String, String]): Encoder[A] =
    self
      .apply(_)
      .transformFields(label => f.applyOrElse(label, identity[String]))

  final def withFieldsRenamedDeep(f: PartialFunction[String, String]): Encoder[A] =
    self
      .apply(_)
      .transformFieldsDeep(label => f.applyOrElse(label, identity[String]))
}

object Encoder {

  private[bencode] trait AsObject[A] extends Encoder[A] {
    def apply(value: A): BDict
  }

  def apply[A: Encoder]: Encoder[A] = implicitly

  implicit val byteVectorEncoder: Encoder[ByteVector] = Bencode.fromByteVector
  implicit val stringEncoder: Encoder[String]         = Bencode.fromString
  implicit val longEncoder: Encoder[Long]             = BInt.apply
  implicit val intEncoder: Encoder[Int]               = int => BInt(int.toLong)

  implicit val booleanEncoder: Encoder[Boolean] =
    stringEncoder.contramap { bool =>
      if (bool) "true" else "false"
    }

  implicit def encodeSeq[A: Encoder]: Encoder[Seq[A]] = list => Bencode.fromSequence(list.map(Encoder[A].apply))
}
