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
  final case class BString(value: String)              extends Bencode
  final case class BInt(value: Int)                    extends Bencode
  final case class BList(values: Bencode*)             extends Bencode
  final case class BDict(value: Map[BString, Bencode]) extends Bencode

  object BDict {

    def apply(entries: (String, Bencode)*): BDict =
      BDict {
        entries.map { case (key, value) => (BString(key), value) }.toMap
      }
  }

//  def parse(value: String): Option[Bencode] = {
//    def helper(currString: String, acc)
//    value match {
//      case int if int.headOption.contains('i') => parseInt(int)
//      case string if string.headOption.exists(_.isDigit) =>
//        val sizePart = string.takeWhile(_ != ':')
//        for {
//          size <- sizePart.toIntOption
//          text = string.drop(sizePart.length + 1).take(size)
//          checkedText <- Option.when(text.length == size)(text)
//        } yield BString(checkedText)
//    }
//  }

  def parseBList(value: String): Option[BList] = {
    def helper(curr: String, acc: List[Bencode]): Option[BList] = curr match {
      case int if parseInt(curr).isDefined =>
        (for {
          parsed <- parseInt(int)
          length = parsed.stringify.length
        } yield helper(curr.drop(length), acc :+ parsed)).flatten
      case string if parseBString(curr).isDefined =>
        (for {
          parsed <- parseBString(string)
          length = parsed.stringify.length
        } yield helper(curr.drop(length), acc :+ parsed)).flatten
      case list if list.headOption.contains('l') =>
        (for {
          parsed <- parseBList(list)
          length = parsed.stringify.length
        } yield helper(curr.drop(length), acc :+ parsed)).flatten
      case "e" => Some(BList(acc: _*))
    }
    helper(value.drop(1), Nil)
  }

  private def listPartString(value: String): String =
    ???

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

}
