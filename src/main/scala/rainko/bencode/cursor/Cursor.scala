package rainko.bencode.cursor

import rainko.bencode.Bencode._
import rainko.bencode.cursor.Cursor.{DictFieldTarget, ListIndexTarget, Target}
import rainko.bencode.{Bencode, Decoder}

import scala.collection.immutable.Queue

final case class Cursor(private val bencode: Bencode, private val targets: Queue[Target]) {
  def list(index: Int): Cursor   = withNextTarget(ListIndexTarget(index))
  def field(key: String): Cursor = withNextTarget(DictFieldTarget(key))

  def focus: Option[Bencode] =
    targets.foldLeft(Option(bencode)) { (bencode, target) =>
      bencode.flatMap(bencode => accessBencode(bencode, target))
    }

  def focusAs[A <: Bencode: Caster]: Option[A] = focus.flatMap(Caster[A].cast)

  def as[A: Decoder]: Either[String, A] =
    focus
      .toRight("Couldn't get to the requested field")
      .flatMap(Decoder[A].apply)

  private def accessBencode(bencode: Bencode, target: Target): Option[Bencode] =
    target match {
      case ListIndexTarget(idx) =>
        Caster[BList]
          .cast(bencode)
          .flatMap(_.values.drop(idx).headOption)
      case DictFieldTarget(field) => Caster[BDict].cast(bencode).flatMap(_.getBencode(field))
    }

  private def withNextTarget(target: Target) = Cursor(bencode, targets.enqueue(target))
}

object Cursor {
  sealed trait Target
  case class ListIndexTarget(index: Int)  extends Target
  case class DictFieldTarget(key: String) extends Target
}
