package rainko
package rainko.bencode

import Bencode._
import syntax._

object Main extends App {
  val personDec = implicitly[Decoder[Person]]
  val person = Person("Aleksander", "Rainko", 21)
  val encodedPerson = person.encode
  val stringifiedPerson = encodedPerson.stringify
  val parsedPerson = Bencode.parse(stringifiedPerson)
  val decoded = parsedPerson.toRight("WRONG").flatMap(ben => personDec(ben))
  println(decoded)
}
