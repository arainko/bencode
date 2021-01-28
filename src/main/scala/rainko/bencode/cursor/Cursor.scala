package rainko.bencode.cursor

import rainko.bencode.cursor.Cursor.Target
import rainko.bencode.cursor.Cursor.ListTarget
import rainko.bencode.cursor.Cursor.ListIndexTarget
import rainko.bencode.cursor.Cursor.IntTarget
import rainko.bencode.cursor.Cursor.StringTarget
import rainko.bencode.cursor.Cursor.DictTarget
import rainko.bencode.cursor.Cursor.DictFieldTarget
import scala.collection.immutable.Queue
import rainko.bencode.Bencode

final case class Cursor(targets: Queue[Target]) {
    def list: Cursor = withNextTarget(ListTarget)
    def list(index: Int): Cursor = withNextTarget(ListIndexTarget(index))
    def int: Cursor = withNextTarget(IntTarget)
    def string: Cursor = withNextTarget(StringTarget)
    def dict: Cursor = withNextTarget(DictTarget)
    def dict(field: String) = withNextTarget(DictFieldTarget(field))

    def isEmpty: Boolean = targets.isEmpty

    def traverseBencode(becode: Bencode): Option[Bencode] = {
        def helper(curr: Option[Bencode], cursor: Cursor): Option[Bencode] = 
            if (this.isEmpty) curr
            else helper(curr.flatMap(accessBencode))
              
    }

    private def accessBencode(bencode: Bencode): Option[Bencode] = this.targets.last match {
        case IntTarget => bencode.int
        case StringTarget => bencode.string
        case ListTarget => bencode.list
        case ListIndexTarget(idx) => bencode.list.flatMap(_.values.drop(idx).headOption)
        case DictTarget => bencode.dict
        case DictFieldTarget(field) => bencode.dict.flatMap(_.getBencode(field))
    }

    private def withNextTarget(target: Target) = Cursor(targets.enqueue(target))
}

object Cursor {

  sealed trait Target
  case object IntTarget extends Target
  case object StringTarget extends Target
  case object ListTarget extends Target
  case class ListIndexTarget(index: Int) extends Target
  case object DictTarget extends Target
  case class DictFieldTarget(key: String) extends Target

}
