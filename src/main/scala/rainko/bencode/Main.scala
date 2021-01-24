package rainko.bencode

import rainko.bencode.derivation.decoder._
import rainko.bencode.derivation.encoder._
import rainko.bencode.syntax._

object Main extends App {
  val cos = Person("Adam", "Wensker", 19, Bruh("BRUH", 1))
  val parsed = Bencode.parse("d4:name4:adam7:surname1:a3:agei3ee")
  val encoder = cos.encode
  println(parsed.map(_.decode[Person]))
}
