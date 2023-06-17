package io.github.arainko.bencode.internal

import scala.reflect.ClassTag
import cats.syntax.all.*
import cats.free.FreeApplicative
import io.github.arainko.bencode.Codec.Field
import cats.arrow.FunctionK
import scala.annotation.nowarn
import scodec.bits.ByteVector
import io.github.arainko.bencode.*

object Decoder:
  def decode[A](codec: Codec[A])(bencode: Bencode): Either[Codec.Error, A] =
    codec match
      case Codec.Identity   => Right(bencode)
      case Codec.Long       => specificBencode[Bencode.Long](bencode).map(_.value)
      case Codec.ByteVector => specificBencode[Bencode.String](bencode).map(_.value)
      case Codec.Vector(codec) =>
        specificBencode[Bencode.List](bencode).flatMap(list => list.values.traverse(decode(codec)))
      case Codec.Dict(codec) =>
        specificBencode[Bencode.Dict](bencode).flatMap(dict => dict.values.traverse(decode(codec)))
      case Codec.Product(fieldComposition) => decodeProduct(fieldComposition)(bencode)
      case Codec.Transformed(transformation) =>
        decode(transformation.codec)(bencode).flatMap(transformation.to)

  private def specificBencode[A <: Bencode](bencode: Bencode)(using tag: ClassTag[A]) =
    Prism.derive[Bencode, A].tryGet(bencode).toRight(Codec.Error(s"Expected ${tag.runtimeClass.getSimpleName}"))

  private def decodeProduct[A](fieldComposition: FreeApplicative[Codec.Field[A, _], A])(
    bencode: Bencode
  ): Either[Codec.Error, A] =
    @nowarn("msg=unused")
    val decodeFn: [fieldTpe] => (Codec.Field[A, fieldTpe], Map[ByteVector, Bencode]) => Either[Codec.Error, fieldTpe] =
      [fieldTpe] =>
        (field: Codec.Field[A, fieldTpe], fields: Map[ByteVector, Bencode]) =>
          field match
            case Field.Required(name, fieldCodec, getter) =>
              fields
                .get(ByteVector.view(name.getBytes("utf-8")))
                .toRight(Codec.Error(s"Field '$name' not found"))
                .flatMap(decode(fieldCodec))
            case Field.Optional(name, fieldCodec, getter) =>
              fields.get(ByteVector.view(name.getBytes("utf-8"))).traverse(decode(fieldCodec))

    specificBencode[Bencode.Dict](bencode).flatMap: dict =>
      val applied = [fieldTpe] => (field: Codec.Field[A, fieldTpe]) => decodeFn(field, dict.values)
      fieldComposition.foldMap(FunctionK.lift(applied))
