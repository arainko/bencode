package rainko.bencode.derivation

import rainko.bencode.{Bencode, Encoder}
import shapeless._
import shapeless.labelled.FieldType
import scala.collection.immutable.SortedMap

abstract class ReprEncoder[A] extends Encoder.AsObject[A]

object ReprEncoder {

  private def dict(fields: (String, Bencode)*) = Bencode.BDict(SortedMap(fields: _*))

  implicit val baseCaseProduct: ReprEncoder[HNil] = _ => dict()

  implicit def inductiveCaseProduct[K <: Symbol, H, T <: HList](implicit
    label: Witness.Aux[K],
    headEncoder: Encoder[H],
    tailEncoder: ReprEncoder[T]
  ): ReprEncoder[FieldType[K, H] :: T] = { case head :: tail =>
    Bencode.BDict {
      tailEncoder(tail).fields + (label.value.name -> headEncoder(head))
    }
  }

  implicit val baseCaseCoproduct: ReprEncoder[CNil] = _ =>
    throw new RuntimeException("Reached CNil while encoding, that is not good and shouldn't happen...")

  implicit def inductiveCaseCoproduct[K <: Symbol, H, T <: Coproduct](implicit
    label: Witness.Aux[K],
    headEncoder: Encoder[H],
    tailEncoder: => ReprEncoder[T]
  ): ReprEncoder[FieldType[K, H] :+: T] =
    new ReprEncoder[FieldType[K, H] :+: T] {
      private[this] lazy val cachedTailEncoder: Encoder.AsObject[T] = tailEncoder

      override def apply(value: FieldType[K, H] :+: T): Bencode.BDict =
        value match {
          case Inl(head) => dict(label.value.name -> headEncoder(head))
          case Inr(tail) => cachedTailEncoder(tail)
        }

    }
}
