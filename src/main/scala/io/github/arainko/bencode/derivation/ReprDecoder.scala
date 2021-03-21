package io.github.arainko.bencode.derivation

import io.github.arainko.bencode._
import shapeless._
import shapeless.labelled._

abstract class ReprDecoder[A] extends Decoder[A]

object ReprDecoder {

  implicit val baseCaseProduct: ReprDecoder[HNil] = _ => Right(HNil)

  implicit def inductiveCaseProduct[K <: Symbol, H, T <: HList](implicit
    label: Witness.Aux[K],
    headDecoder: Decoder[H],
    tailDecoder: ReprDecoder[T]
  ): ReprDecoder[FieldType[K, H] :: T] =
    bencode =>
      for {
        head <- bencode.cursor.field(label.value.name).as[H](headDecoder)
        tail <- tailDecoder(bencode)
      } yield field[K](head) :: tail

  implicit val baseCaseCoproduct: ReprDecoder[CNil] = _ => Left(BencodeError.UnexpectedValue("Expansion reached CNil"))

  implicit def inductiveCaseCoproduct[K <: Symbol, H, T <: Coproduct](implicit
    label: Witness.Aux[K],
    headDecoder: Decoder[H],
    tailDecoder: => ReprDecoder[T]
  ): ReprDecoder[FieldType[K, H] :+: T] =
    new ReprDecoder[FieldType[K, H] :+: T] {
      private[this] lazy val cachedTailDecoder: Decoder[T] = tailDecoder

      def apply(bencode: Bencode): Either[DecodingError, FieldType[K, H] :+: T] =
        bencode.cursor.field(label.value.name).focus match {
          case Some(value) => value.cursor.as[H].map(l => Inl(field(l)))
          case None        => cachedTailDecoder(bencode).map(Inr.apply)
        }
    }
}
