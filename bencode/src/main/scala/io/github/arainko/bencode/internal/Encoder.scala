package io.github.arainko.bencode.internal

import cats.MonoidK
import cats.arrow.FunctionK
import cats.free.FreeApplicative
import cats.kernel.Monoid
import io.github.arainko.bencode.Codec.Field

import scala.collection.immutable.SortedMap
import cats.kernel.Order
import scodec.bits.ByteVector
import io.github.arainko.bencode.*


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

  private given Order[ByteVector] = Order.fromOrdering
  private given Monoid[SortedMap[ByteVector, Bencode]] = MonoidK[SortedMap[ByteVector, _]].algebra

  private def encodeProduct[A](fieldComposition: FreeApplicative[Codec.Field[A, _], A])(value: A): Bencode =
    val enc = [fieldTpe] =>
      (field: Codec.Field[A, fieldTpe]) =>
        field match
          case Field.Required(name, fieldCodec, getter) =>
            SortedMap(ByteVector.view(name.getBytes("utf-8")) -> encode(fieldCodec)(getter(value)))
          case Field.Optional(name, fieldCodec, getter) =>
            getter(value)
              .map(field => SortedMap(ByteVector.view(name.getBytes("utf-8")) -> encode(fieldCodec)(field)))
              .getOrElse(SortedMap.empty[ByteVector, Bencode])
              
    Bencode.Dict(fieldComposition.analyze(FunctionK.lift(enc)))
