package rainko.bencode.derivation

import rainko.bencode.Bencode.BDict
import rainko.bencode.{Bencode, Decoder}
import shapeless._
import shapeless.labelled.{FieldType, field}

object decoder {

  trait BObjectDecoder[A] extends Decoder[A] { self =>
    def decodeAsObject(value: BDict): Either[String, A]

    final override def apply(bencode: Bencode): Either[String, A] =
      bencode.dict
        .toRight("Not a dict!")
        .flatMap(self.decodeAsObject)
  }

  implicit val hnilDecoder: BObjectDecoder[HNil] = _ => Right(HNil)

  implicit def hlistObjectDecoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    headDecoder: Lazy[Decoder[H]],
    tailDecoder: BObjectDecoder[T]
  ): BObjectDecoder[FieldType[K, H] :: T] =
    (value: BDict) => for {
      head <- value.get[H](witness.value.name)(headDecoder.value)
      tail <- tailDecoder(value)
    } yield field[K](head) :: tail

  implicit def productAsObjectDecoder[P <: Product, HL <: HList](implicit
    gen: LabelledGeneric.Aux[P, HL],
    encoder: Lazy[BObjectDecoder[HL]]
  ): Decoder[P] = encoder.value.map(gen.from)

}
