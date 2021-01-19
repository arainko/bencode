package rainko
package rainko.bencode

sealed trait Bencode {
  import Bencode._

  final def stringify: String =
    this match {
      case BString(value)     => s"${value.length}:$value"
      case BInt(value)        => s"i${value}e"
      case BList(values @ _*) => s"l${values.map(_.stringify).mkString}e"
      case BDict(values) =>
        values
          .map { case (key, value) => s"${key.stringify}${value.stringify}" }
          .mkString("d", "", "e")
    }
}

object Bencode {
  final case class BString(value: String) extends Bencode
  final case class BInt(value: Int)       extends Bencode

  final case class BList(values: Bencode*) extends Bencode {
    override def toString: String = s"Blist(${values.mkString(", ")})"
  }
  final case class BDict(value: Map[BString, Bencode]) extends Bencode

  object BDict {

    def apply(entries: (String, Bencode)*): BDict =
      BDict {
        entries.map { case (key, value) => (BString(key), value) }.toMap
      }
  }

  def parseBList(value: String): Option[BList] = {
    def helper(curr: String, acc: List[Bencode]): Option[BList] =
      curr match {
        case int if int.headOption.contains('i') =>
          parseInt(int).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            helper(curr.drop(skippedLength), acc :+ parsed)
          }
        case string if string.headOption.exists(_.isDigit) =>
          parseBString(string).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            helper(curr.drop(skippedLength), acc :+ parsed)
          }
        case list if list.headOption.contains('l') =>
          parseBList(list).flatMap { parsed =>
            val skippedLength = skipSize(parsed)
            helper(curr.drop(skippedLength), acc :+ parsed)
          }
        case endList if endList.headOption.contains('e') => Some(BList(acc: _*))
      }
    helper(value.drop(1), Nil)
  }

//  private def parseBDict(value: String): Option[BDict] = {
//    def helper(curr: String, acc: List[(BString, Bencode)]): Option[BDict] =
//      curr match {
//        case int if int.headOption.contains('i') =>
//          parseInt(int).flatMap { parsed =>
//            val skippedLength = skipSize(parsed)
//            helper(curr.drop(skippedLength), acc :+ parsed)
//          }
//        case string if string.headOption.exists(_.isDigit) =>
//          parseBString(string).flatMap { parsed =>
//            val skippedLength = skipSize(parsed)
//            helper(curr.drop(skippedLength), acc :+ parsed)
//          }
//        case list if list.headOption.contains('l') =>
//          parseBList(list).flatMap { parsed =>
//            val skippedLength = skipSize(parsed)
//            helper(curr.drop(skippedLength), acc :+ parsed)
//          }
//        case endList if endList.headOption.contains('e') => Some(BList(acc: _*))
//      }
//
//  }

  private def parseInt(value: String) =
    value.drop(1).takeWhile(_ != 'e').toIntOption.map(BInt)

  private def parseBString(string: String) = {
    val sizePart = string.takeWhile(_ != ':')
    for {
      size <- sizePart.toIntOption
      text = string.drop(sizePart.length + 1).take(size)
      checkedText <- Option.when(text.length == size)(text)
    } yield BString(checkedText)
  }

  private def skipSize(bencode: Bencode): Int =
    bencode match {
      case BString(value)     => value.length + value.length.toString.length + 1
      case BInt(value)        => value.toString.length + 2
      case BList(values @ _*) => values.foldLeft(0)((acc, curr) => acc + skipSize(curr)) + 2
      case BDict(value)       => value.foldLeft(0)((acc, curr) => acc + skipSize(curr._1) + skipSize(curr._2)) + 2
    }

}
