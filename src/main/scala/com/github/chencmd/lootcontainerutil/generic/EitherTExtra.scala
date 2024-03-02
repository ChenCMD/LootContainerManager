package com.github.chencmd.lootcontainerutil.generic

import cats.Applicative
import cats.Functor
import cats.data.EitherT
import cats.implicits.*

object EitherTExtra {
  class ExitWhenFOps[F[_]](cond: Boolean) extends AnyVal {
    def apply[A](exitValue: => F[A])(using F: Functor[F]): EitherT[F, A, Unit] = {
      EitherT(exitValue.map(Either.cond(cond, (), _)))
    }
  }

  class ExitWhenAOps[F[_]](cond: Boolean) extends AnyVal {
    def apply[A](exitValue: => A)(using M: Applicative[F]): EitherT[F, A, Unit] = {
      EitherT.fromEither(Either.cond(cond, (), exitValue))
    }
  }

  class ExitWhenMAOps[F[_]](cond: F[Boolean]) extends AnyVal {
    def apply[A](exitValue: => A)(using F: Functor[F]): EitherT[F, A, Unit] = {
      EitherT(cond.map(Either.cond(_, (), exitValue)))
    }
  }

  class ExitWhenMFOps[F[_]](cond: F[Boolean]) extends AnyVal {
    def apply[A](exitValue: => F[A])(using M: Applicative[F]): EitherT[F, A, Unit] = {
      EitherT(cond.map2(exitValue)(Either.cond(_, (), _)))
    }
  }

  def exitWhenF[F[_]](cond: Boolean) = new ExitWhenFOps(cond)

  def exitWhenA[F[_]](cond: Boolean) = new ExitWhenAOps[F](cond)

  def exitWhenMA[F[_]](cond: F[Boolean]) = new ExitWhenMAOps(cond)

  def exitWhenMF[F[_]](cond: F[Boolean]) = new ExitWhenMFOps(cond)
}
