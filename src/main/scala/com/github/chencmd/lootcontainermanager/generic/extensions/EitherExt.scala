package com.github.chencmd.lootcontainermanager.generic.extensions

import cats.ApplicativeError
import cats.implicits.*

object EitherExt {
  class OrRaiseFOps[F[_], A, B](either: Either[A, B]) extends AnyVal {
    def apply[E, E2 <: E](e: A => E2)(using AE: ApplicativeError[F, E]): F[B] = {
      either.fold(a => AE.raiseError(e(a)), _.pure[F])
    }
    def apply[E, E2 <: E](e: => E2)(using AE: ApplicativeError[F, E]): F[B]   = {
      either.fold(_ => AE.raiseError(e), _.pure[F])
    }
  }

  extension [A, B](optionA: Either[A, B]) {
    def orRaiseF[F[_]] = new OrRaiseFOps[F, A, B](optionA)
  }
}
