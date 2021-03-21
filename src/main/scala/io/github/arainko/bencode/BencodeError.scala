package io.github.arainko.bencode

sealed trait BencodeError

sealed trait DecodingError extends BencodeError

object BencodeError {
  final case class ParsingFailure(message: String, failedInput: String) extends BencodeError

  def parsingFailure(typeName: String, input: String): ParsingFailure =
    ParsingFailure(s"Couldn't parse $typeName for input: ${input.take(15)}...", input)

  final case class UnexpectedValue(message: String) extends DecodingError
}
