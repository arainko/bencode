package rainko
package rainko.bencode

import Bencode._
import syntax._

object Main extends App {

  val bval =
    BList(1.encode, "dupaaaal".encode, BList(1.encode, BList(1.encode, 2.encode, 10.encode), 3.encode), "asdasd".encode)
  val stringified = bval.stringify
  val parsed      = Bencode.parseBList(bval.stringify + "asd")
  println(parsed)
}
