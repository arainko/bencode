package io.github.arainko.bencode.derivation

import io.github.arainko.bencode.{Bencode, Decoder, DecodingError}
import shapeless.LabelledGeneric

abstract class DerivedDecoder[A] extends Decoder[A]

object DerivedDecoder {

  implicit def deriveDecoder[A, R](implicit
    repr: LabelledGeneric.Aux[A, R],
    reprDecoder: => ReprDecoder[R]
  ): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      private[this] lazy val cachedReprDecoder: Decoder[R] = reprDecoder

      def apply(bencode: Bencode): Either[DecodingError, A] =
        cachedReprDecoder(bencode) match {
          case Right(value) => Right(repr.from(value))
          case l @ Left(_)  => l.asInstanceOf[Either[DecodingError, A]]
        }
    }
}
