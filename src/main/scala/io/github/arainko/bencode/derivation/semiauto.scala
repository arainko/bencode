package io.github.arainko.bencode.derivation

import io.github.arainko.bencode.{Decoder, Encoder}

object semiauto {
  final def deriveDecoder[A](implicit decoder: DerivedDecoder[A]): Decoder[A]          = decoder
  final def deriveEncoder[A](implicit encoder: DerivedEncoder[A]): Encoder.AsObject[A] = encoder
}
