package rainko.bencode.derivation

import rainko.bencode.Bencode.{BDict, BString}
import rainko.bencode.Encoder
import shapeless.labelled.FieldType
import shapeless._

object encoder {

  trait BObjectEncoder[A] extends Encoder[A] {
    def apply(value: A): BDict
  }

  implicit val hnilEncoder: BObjectEncoder[HNil] = _ => BDict()

  implicit val cnilEncoder: BObjectEncoder[CNil] = _ =>
    throw new RuntimeException("CNil is an uninhabited type and cannot be constructed!")

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    headEncoder: Lazy[Encoder[H]],
    tailEncoder: BObjectEncoder[T]
  ): BObjectEncoder[FieldType[K, H] :: T] =
    hlist => {
      val label       = witness.value.name
      val encodedHead = headEncoder.value.apply(hlist.head)
      val encodedTail = tailEncoder.apply(hlist.tail)
      BDict(encodedTail.fields + (BString(label) -> encodedHead))
    }

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
    witness: Witness.Aux[K],
    headEncoder: Lazy[Encoder[H]],
    tailEncoder: BObjectEncoder[T]
  ): BObjectEncoder[FieldType[K, H] :+: T] = {
    case Inl(head) =>
      BDict(witness.value.name -> headEncoder.value.apply(head))
    case Inr(tail) => tailEncoder.apply(tail)
  }

  implicit def productAsObjectEncoder[P <: Product, HL <: HList](implicit
    gen: LabelledGeneric.Aux[P, HL],
    encoder: Lazy[BObjectEncoder[HL]]
  ): BObjectEncoder[P] = value => encoder.value.apply(gen.to(value))

  implicit def coproductAsObjectEncoder[P, C <: Coproduct](implicit
    gen: LabelledGeneric.Aux[P, C],
    encoder: Lazy[BObjectEncoder[C]]
  ): BObjectEncoder[P] = value => encoder.value.apply(gen.to(value))
}
