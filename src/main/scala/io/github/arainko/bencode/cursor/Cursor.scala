package io.github.arainko.bencode.cursor

import io.github.arainko.bencode.Bencode._
import io.github.arainko.bencode.cursor.Cursor.{DictFieldTarget, ListIndexTarget, Target}
import io.github.arainko.bencode.{Bencode, Decoder, DecodingError}

import scala.collection.immutable.Queue

final case class Cursor(private val bencode: Bencode, private val targets: Queue[Target]) {
  def list(index: Int): Cursor   = withNextTarget(ListIndexTarget(index))
  def field(key: String): Cursor = withNextTarget(DictFieldTarget(key))

  def focus: Option[Bencode] =
    targets.foldLeft(bencode)(downTree) match {
      case BEmpty => None
      case other  => Some(other)
    }

  def as[A: Decoder]: Either[DecodingError, A] =
    Decoder[A].apply {
      targets.foldLeft(bencode)(downTree)
    }

  private def downTree(bencode: Bencode, target: Target) =
    target match {
      case ListIndexTarget(idx) =>
        Caster[BList]
          .cast(bencode)
          .flatMap(_.values.drop(idx).headOption)
          .getOrElse(BEmpty)

      case DictFieldTarget(field) =>
        Caster[BDict]
          .cast(bencode)
          .flatMap(_.fields.get(field))
          .getOrElse(BEmpty)
    }

  private def withNextTarget(target: Target) = Cursor(bencode, targets.enqueue(target))
}

object Cursor {
  sealed trait Target
  case class ListIndexTarget(index: Int)  extends Target
  case class DictFieldTarget(key: String) extends Target
}
