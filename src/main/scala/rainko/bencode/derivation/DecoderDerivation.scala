package rainko.bencode.derivation

import rainko.bencode.Bencode.BDict
import rainko.bencode.Decoder
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.annotation.nowarn

private[derivation] trait DecoderDerivation {

  implicit val hnilDecoder: Decoder.AsObject[HNil] = _ => Right(HNil)

  implicit val cnilDecoder: Decoder.AsObject[CNil] = _ => Left("CNil!")

  implicit def hlistObjectDecoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    headDecoder: Lazy[Decoder[H]],
    tailDecoder: Decoder.AsObject[T]
  ): Decoder.AsObject[FieldType[K, H] :: T] =
    (value: BDict) =>
      for {
        head <- value.cursor.field(witness.value.name).as[H](headDecoder.value)
        tail <- tailDecoder(value)
      } yield field[K](head) :: tail

  implicit def coproductObjectDecoder[K <: Symbol, H, T <: Coproduct](implicit
    witness: Witness.Aux[K],
    headDecoder: Lazy[Decoder[H]],
    tailDecoder: Decoder.AsObject[T]
  ): Decoder.AsObject[FieldType[K, H] :+: T] =
    (value: BDict) =>
      value.cursor.field(witness.value.name).as[H](headDecoder.value) match {
        case Right(value) => Right(Inl(field[K](value)))
        case Left(_)      => tailDecoder(value).map(Inr.apply)
      }

  implicit def coproductAsObjectDecoder[P, C <: Coproduct](implicit
    gen: LabelledGeneric.Aux[P, C],
    encoder: Lazy[Decoder.AsObject[C]],
    @nowarn lp: LowPriority
  ): Decoder[P] = encoder.value.map(gen.from)

  implicit def productAsObjectDecoder[P <: Product, HL <: HList](implicit
    gen: LabelledGeneric.Aux[P, HL],
    encoder: Lazy[Decoder.AsObject[HL]],
    @nowarn lp: LowPriority
  ): Decoder[P] = encoder.value.map(gen.from)

}
