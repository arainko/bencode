package io.github.arainko.bencode

import scodec.*
import scodec.bits.*
import scala.collection.immutable.SortedMap
import scala.deriving.Mirror.Sum
import io.github.arainko.bencode.internal.Parser
import io.github.arainko.bencode.internal.PrettyPrinter
import java.lang

enum Bencode:

  final def byteify: ByteVector = Parser.unparse(this)

  final def prettyPrint: java.lang.String = PrettyPrinter.show(this)

  // bencode of eg. 100 repr: i100e
  case Long(value: scala.Long)

  // bencode repr of 'example': 7:example
  case String(value: ByteVector)

  // bencode repr of List("spam"): l4:spami42ee
  case List(values: Vector[Bencode])

  // bencode dicts are supposed to have stable order
  // bencode repr of Map("field1" -> 123): d6:field1i123ee
  case Dict(values: SortedMap[ByteVector, Bencode])
