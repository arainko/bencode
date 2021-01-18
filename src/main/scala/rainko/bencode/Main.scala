package rainko
package rainko.bencode

import Bencode._
import syntax._

object Main extends App {
 val bval = BList(1.encode, "dupaaaal".encode, "asdasd".encode)
  val stringified = bval.stringify
  val parsed = Bencode.parseBList(bval.stringify)
  println(parsed)
}
