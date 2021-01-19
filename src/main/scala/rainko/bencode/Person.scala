package rainko
package rainko.bencode

import Bencode._

case class Person(name: String, surname: String, age: Int)

object Person {

  implicit val personEncoder = new Encoder[Person] {

    override def apply(value: Person): Bencode =
      BDict(
        "name"    -> BString(value.name),
        "surname" -> BString(value.surname),
        "age"     -> BInt(value.age)
      )
  }

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
