package rainko.bencode.derivation

import rainko.bencode.Bencode.BDict
import rainko.bencode.{Bencode, Encoder}
import shapeless._
import shapeless.labelled.FieldType

private[derivation] trait EncoderDerivation {

  private def dict(fields: (String, Bencode)*) = BDict(fields.toMap)

  implicit val hnilEncoder: Encoder.AsObject[HNil] = _ => dict()

  implicit val cnilEncoder: Encoder.AsObject[CNil] = _ =>
    throw new RuntimeException("CNil is an uninhabited type and cannot be constructed!")

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    headEncoder: Lazy[Encoder[H]],
    tailEncoder: Encoder.AsObject[T]
  ): Encoder.AsObject[FieldType[K, H] :: T] =
    hlist => {
      val label       = witness.value.name
      val encodedHead = headEncoder.value.apply(hlist.head)
      val encodedTail = tailEncoder.apply(hlist.tail)
      BDict(encodedTail.fields + (label -> encodedHead))
    }

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
    witness: Witness.Aux[K],
    headEncoder: Lazy[Encoder[H]],
    tailEncoder: Encoder.AsObject[T]
  ): Encoder.AsObject[FieldType[K, H] :+: T] = {
    case Inl(head) => dict(witness.value.name -> headEncoder.value.apply(head))
    case Inr(tail) => tailEncoder.apply(tail)
  }

  implicit def productAsObjectEncoder[P <: Product, HL <: HList](implicit
    gen: LabelledGeneric.Aux[P, HL],
    encoder: Lazy[Encoder.AsObject[HL]]
  ): Encoder.AsObject[P] = value => encoder.value.apply(gen.to(value))

  implicit def coproductAsObjectEncoder[P, C <: Coproduct](implicit
    gen: LabelledGeneric.Aux[P, C],
    encoder: Lazy[Encoder.AsObject[C]]
  ): Encoder.AsObject[P] = value => encoder.value.apply(gen.to(value))
}
