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

  def parse(value: String): Option[Bencode] = {
    def helper(currString: String, acc)
    value match {
      case int if int.headOption.contains('i') => parseInt(int)
      case string if string.headOption.exists(_.isDigit) =>
        val sizePart = string.takeWhile(_ != ':')
        for {
          size <- sizePart.toIntOption
          text = string.drop(sizePart.length + 1).take(size)
          checkedText <- Option.when(text.length == size)(text)
        } yield BString(checkedText)
    }
  }

  private def listPartString(value: String): String =
    ???

  private def parseInt(value: String) =
    value.drop(1).takeWhile(_ != 'e').toIntOption.map(BInt)
}
