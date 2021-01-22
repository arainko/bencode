package rainko.bencode

import Bencode._

case class Person(name: String, surname: String, age: Int)

object Person {
  implicit val personDecoder: Decoder[Person] = (bencode: Bencode) => {
    val map = bencode.asInstanceOf[BDict].value
    (for {
      name    <- map.get(BString("name"))
      surname <- map.get(BString("surname"))
      age     <- map.get(BString("age"))
    } yield Person(name.asInstanceOf[BString].value, surname.asInstanceOf[BString].value, age.asInstanceOf[BInt].value))
      .toRight("WRONG")
  }
}
