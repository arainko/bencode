package rainko.bencode

import rainko.bencode.derivation.encoder._
import rainko.bencode.derivation.decoder._
import rainko.bencode.syntax._

object Main extends App {

  sealed trait Costam
  final case class Costam1(cos: String, cos2: Int) extends Costam
  final case class Costam2(cos3: String, cos4: Int) extends Costam

  val cos: Option[Int] = None

  println(cos.encode)
  println(cos.encode.decode[Option[Int]])
}
