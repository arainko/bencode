package rainko.bencode

sealed trait BencodeError

object BencodeError {
  final case class ParsingFailure(message: String) extends BencodeError
  final case class DecodingError(message: String) extends BencodeError

  def parsingFailure(typeName: String, input: String) =
    ParsingFailure(s"Couldn't parse $typeName for input: ")
}
