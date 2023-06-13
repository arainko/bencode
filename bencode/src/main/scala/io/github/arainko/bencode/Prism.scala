package io.github.arainko.bencode

import scala.reflect.ClassTag

// copied over from here: https://github.com/SystemFw/dynosaur/blob/main/modules/core/shared/src/main/scala/Prism.scala
case class Prism[A, B](tryGet: A => Option[B], inject: B => A)

object Prism extends LowPrioPrism:

  def fromPartial[A, B](tryGet: PartialFunction[A, B])(inject: B => A) =
    Prism(tryGet.lift, inject)

  given identity[A]: Prism[A, A] =
    Prism[A, A](Option.apply, a => a)

  given left[A, B]: Prism[Either[A, B], Left[A, B]] =
    Prism.fromPartial[Either[A, B], Left[A, B]] { case v @ Left(_) =>
      v
    }(v => v)

  given right[A, B]: Prism[Either[A, B], Right[A, B]] =
    Prism.fromPartial[Either[A, B], Right[A, B]] { case v @ Right(_) =>
      v
    }(v => v)

  given none[A]: Prism[Option[A], None.type] =
    Prism.fromPartial[Option[A], None.type] { case v: None.type =>
      v
    }(none => none)

  given some[A]: Prism[Option[A], Some[A]] =
    Prism.fromPartial[Option[A], Some[A]] { case v @ Some(_) =>
      v
    }(v => v)

private[bencode] sealed trait LowPrioPrism:

  given derive[S <: Matchable, A <: S](using
      tag: ClassTag[A]
  ): Prism[S, A] =
    val tryGet = (s: S) =>
      PartialFunction.condOpt(s): 
        case tag(inst) => inst

    val inject = (a: A) => (a: S)

    Prism(tryGet, inject)
