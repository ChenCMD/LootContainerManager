package com.github.chencmd.lootcontainermanager.generic.extensions

import cats.Applicative
import cats.ApplicativeError
import cats.implicits.*

object OptionExt {
  class OrRaiseFOps[F[_], A](optionA: Option[A]) extends AnyVal {
    def apply[E, E2 <: E](e: => E2)(using AE: ApplicativeError[F, E]): F[A] = optionA.fold(AE.raiseError(e))(_.pure[F])
  }

  extension [A](optionA: Option[A]) {
    def getOrElseEffectfully[F[_]: Applicative](fa: F[A]): F[A] = optionA.fold(fa)(_.pure[F])

    def orRaiseF[F[_]] = new OrRaiseFOps[F, A](optionA)
  }
}
