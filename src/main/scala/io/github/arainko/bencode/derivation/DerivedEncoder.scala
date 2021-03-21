package io.github.arainko.bencode.derivation

import io.github.arainko.bencode.{Bencode, Encoder}
import shapeless.LabelledGeneric

abstract class DerivedEncoder[A] extends Encoder.AsObject[A]

object DerivedEncoder {

  implicit def deriveEncoder[A, R](implicit
    repr: LabelledGeneric.Aux[A, R],
    reprEncoder: => ReprEncoder[R]
  ): DerivedEncoder[A] =
    new DerivedEncoder[A] {
      private[this] lazy val cachedReprEncoder: Encoder.AsObject[R] = reprEncoder

      override def apply(value: A): Bencode.BDict = cachedReprEncoder(repr.to(value))
    }
}
