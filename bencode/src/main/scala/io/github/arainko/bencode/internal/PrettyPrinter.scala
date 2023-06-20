package io.github.arainko.bencode.internal

import io.github.arainko.bencode.Bencode
import scala.collection.mutable

object PrettyPrinter {
  def show(bencode: Bencode): String =
    def ident(times: Int) = " " * times
    val newline = "\n"

    def loop(depth: Int, builder: StringBuilder, current: Bencode): builder.type =
      current match
        case Bencode.Long(value) =>
          builder.append("i").append(value).append("e")

        case Bencode.String(value) =>
          builder.append(value.size).append(":").append(value.decodeUtf8.getOrElse(s"<blob>"))

        case Bencode.List(values) =>
          builder.append(newline).append(ident(depth + 1)).append("l")
          values.foreach: bencode =>
            builder
              .append(newline)
              .append(ident(depth + 2))
            loop(depth + 2, builder, bencode)
            builder.append(", ")
          builder.append(newline).append(ident(depth + 1)).append("e")

        case Bencode.Dict(values) =>
          builder.append(newline).append(ident(depth + 1)).append("d")
          values.foreach: (key, value) =>
            builder
              .append(newline)
              .append(ident(depth + 2))
              .append(key.size)
              .append(":")
              .append(String(key.toArray))
              .append(" -> ")
            loop(depth + 2, builder, value)

          builder.append(newline).append(ident(depth + 1)).append("e")

    loop(0, StringBuilder(), bencode).result()

}
