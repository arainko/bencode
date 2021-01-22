package rainko.bencode

import shapeless._
import rainko.bencode.syntax.EncoderOps
import rainko.bencode.derivation._

object Main extends App {
  val cos = (1, 2, 3)
  val encoder = cos.encode
  println(encoder)
}
