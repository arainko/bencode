package io.github.arainko.bencode

import cats.instances.tuple._
import cats.syntax.bifunctor._
import io.github.arainko.bencode.Bencode._
import scodec.bits.ByteVector

trait Encoder[A] { self =>
  def apply(value: A): Bencode

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.apply(f(value))

  final def transform(f: Bencode => Bencode): Encoder[A] = value => f(self.apply(value))

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
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = Bencode.fromByteArray
  implicit val stringEncoder: Encoder[String]         = Bencode.fromString
  implicit val longEncoder: Encoder[Long]             = BInt.apply
  implicit val intEncoder: Encoder[Int]               = int => BInt(int.toLong)

  implicit val booleanEncoder: Encoder[Boolean] =
    stringEncoder.contramap { bool =>
      if (bool) "true" else "false"
    }
  implicit def optionEncoder[A: Encoder]: Encoder[Option[A]] = value => value.map(Encoder[A].apply).getOrElse(BEmpty)

  implicit def encodeSeq[A: Encoder]: Encoder[Seq[A]] = list => Bencode.fromSequence(list.map(Encoder[A].apply))

  implicit def encodeMap[A: Encoder]: Encoder[Map[String, A]] =
    map => Bencode.fromMap(map.map(_.bimap(identity, Encoder[A].apply)))
}
