package io.github.arainko.bencode

import scala.collection.immutable.SortedMap
import cats.free.FreeApplicative
import cats.MonoidK
import cats.kernel.Monoid
import cats.arrow.FunctionK
import io.github.arainko.bencode.Codec.Field

object Encoder:
  def encode[A](codec: Codec[A])(value: A): Bencode =
    codec match
      case Codec.Identity                    => value
      case Codec.Long                        => Bencode.Long(value)
      case Codec.ByteVector                  => Bencode.String(value)
      case Codec.Vector(codec)               => Bencode.List(value.map(encode(codec)))
      case Codec.Dict(codec)                 => Bencode.Dict(SortedMap.from(value.map { case k -> v => k -> encode(codec)(v) }))
      case Codec.Product(fieldComposition)   => encodeProduct(fieldComposition)(value)
      case Codec.Transformed(transformation) => encode(transformation.codec)(transformation.from(value))

  private def encodeProduct[A](fieldComposition: FreeApplicative[Codec.Field[A, _], A])(value: A): Bencode =
    given Monoid[SortedMap[String, Bencode]] = MonoidK[SortedMap[String, _]].algebra
    val enc = [fieldTpe] =>
      (field: Codec.Field[A, fieldTpe]) =>
        field match
          case Field.Required(name, fieldCodec, getter) =>
            SortedMap(name -> encode(fieldCodec)(getter(value)))
          case Field.Optional(name, fieldCodec, getter) =>
            getter(value)
              .map(field => SortedMap(name -> encode(fieldCodec)(field)))
              .getOrElse(SortedMap.empty[String, Bencode])
    Bencode.Dict(fieldComposition.analyze(FunctionK.lift(enc)))