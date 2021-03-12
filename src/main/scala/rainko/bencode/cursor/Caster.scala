package rainko.bencode.cursor

import rainko.bencode.Bencode
import rainko.bencode.Bencode._

private[cursor] trait Caster[A <: Bencode] {
  def cast(bencode: Bencode): Option[A]
}

object Caster {
  def apply[A <: Bencode: Caster]: Caster[A] = implicitly

  implicit val bintCaster: Caster[BInt] = {
    case int @ BInt(_) => Some(int)
    case _             => None
  }

  implicit val bstringCaster: Caster[BString] = {
    case string @ BString(_) => Some(string)
    case _                   => None
  }

  implicit val blistCaster: Caster[BList] = {
    case list @ BList(_) => Some(list)
    case _               => None
  }

  implicit val bdictCaster: Caster[BDict] = {
    case dict @ BDict(_) => Some(dict)
    case _               => None
  }
}
