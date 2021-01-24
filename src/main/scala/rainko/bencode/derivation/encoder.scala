package rainko.bencode.derivation

import rainko.bencode.Bencode.{BDict, BString}
import rainko.bencode.{Bencode, Decoder, Encoder}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

object encoder {

  trait BObjectEncoder[A] extends Encoder[A] {
    def apply(value: A): BDict
  }

  implicit val hnilEncoder: BObjectEncoder[HNil] = _ => BDict()

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

  implicit def productAsObjectEncoder[P <: Product, HL <: HList](implicit
    gen: LabelledGeneric.Aux[P, HL],
    encoder: Lazy[BObjectEncoder[HL]]
  ): BObjectEncoder[P] = value => encoder.value.apply(gen.to(value))

}