package io.github.arainko.bencode

import scodec.bits.ByteVector
import cats.free.FreeApplicative
import java.lang
import scala.annotation.nowarn

enum Codec[A]:
  self =>

  final def encode(value: A): Bencode = ???
  final def decode(bencode: Bencode): Either[Codec.Error, A] = ???

  final def imap[B](to: A => B)(from: B => A): Codec[B] =
    xmap(to.andThen(Right.apply))(from.andThen(Right.apply))

  final def imapErr[B](to: A => Either[String, B])(from: B => A): Codec[B] =
    xmap(to)(from.andThen(Right.apply))

  final def xmap[B](_to: A => Either[String, B])(_from: B => Either[String, A]): Codec[B] =
    Codec.Transformed:
      new:
        type Repr = A
        val codec: Codec[Repr] = self
        def from(value: B): Either[Codec.Error, Repr] = _from(value).left.map(Codec.Error(_))
        def to(repr: A): Either[Codec.Error, B] = _to(repr).left.map(Codec.Error(_))

  case Identity extends Codec[Bencode]
  case Long extends Codec[scala.Long]
  case ByteVector extends Codec[scodec.bits.ByteVector]
  case Vector[Elem](codec: Codec[Elem]) extends Codec[scala.Vector[Elem]]
  case Dict[Elem](codec: Codec[Elem]) extends Codec[Map[String, Elem]]
  case Product[RecordType](fieldComposition: FreeApplicative[Codec.Field[RecordType, _], RecordType]) extends Codec[RecordType]
  case Transformed[A](transformation: Codec.Transformation[A]) extends Codec[A]

object Codec:

  given identity: Codec[Bencode] = Codec.Identity

  given string: Codec[String] =
    Codec.ByteVector
      .xmap(_.decodeUtf8.left.map(_.getMessage))(str => scodec.bits.ByteVector.encodeUtf8(str).left.map(_.getMessage))

  given long: Codec[Long] = Codec.Long

  given byteVector: Codec[ByteVector] = Codec.ByteVector

  given vector[A](using A: Codec[A]): Codec[scala.Vector[A]] = Codec.Vector(A)

  given map[A](using A: Codec[A]): Codec[Map[String, A]] = Codec.Dict(A)

  def product[A](builder: Field.Builder[A] => FreeApplicative[Codec.Field[A, _], A]): Codec[A] =
    Codec.Product(builder(Field.Builder()))

  def coproduct[A](builed: Case.Builder[A] => FreeApplicative[Codec.Case[A, _], A]): Codec[A] = ???

  enum Field[RecordType, FieldType]:
    case Required[RecordType, FieldType](
      name: String,
      fieldCodec: Codec[FieldType],
      getter: RecordType => FieldType
    ) extends Field[RecordType, FieldType]

    case Optional[RecordType, FieldType](
      name: String,
      fieldCodec: Codec[FieldType],
      getter: RecordType => Option[FieldType]
    ) extends Field[RecordType, Option[FieldType]]

  object Field:
    class Builder[RecordType]:
      def apply[FieldType](
        name: String,
        getter: RecordType => FieldType
      )(using FieldType: Codec[FieldType]): FreeApplicative[Field[RecordType, _], FieldType] =
        FreeApplicative.lift(Field.Required(name, FieldType, getter))

      def opt[FieldType](
        name: String,
        getter: RecordType => Option[FieldType]
      )(using FieldType: Codec[FieldType]): FreeApplicative[Field[RecordType, _], Option[FieldType]] =
        FreeApplicative.lift(Field.Optional(name, FieldType, getter))

      def const[A](name: String, value: A)(using A: Codec[A]): FreeApplicative[Field[RecordType, _], Unit] =
        given Codec[Unit] =
          A.xmap(a => Either.cond(a == value, (), s"$a does not match const $value"))(_ => Right(value))
        apply(name, _ => value): @nowarn("msg=A pure expression does nothing")

  // enum Case 

  trait Transformation[A]:
    type Repr
    def codec: Codec[Repr]
    def from(value: A): Either[Codec.Error, Repr]
    def to(repr: Repr): Either[Codec.Error, A]

  final class Error(message: String) extends Exception:
    override def getMessage(): String = message
    override def toString(): String = message