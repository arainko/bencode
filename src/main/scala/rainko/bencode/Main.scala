package rainko
package rainko.bencode

import Bencode._
import syntax._

object Main extends App {
 val bval = BList(1.encode, "dupaaaal".encode, BList(1.encode, "dupaaaal".encode))
  val stringified = bval.stringify
  println("i123e".headOption.contains('i'))
  val parsed = Bencode.parse("5:dupal")
  println(parsed)
}
