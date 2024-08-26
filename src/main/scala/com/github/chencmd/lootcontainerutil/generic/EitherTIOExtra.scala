package com.github.chencmd.lootcontainermanager.generic

import cats.data.EitherT
import cats.effect.IO
import cats.implicits.*

object EitherTIOExtra {
  trait ErrorProcessor[E, A] {
    def processError(err: E): IO[A]
  }

  extension [E, A](eitherT: EitherT[IO, E, A]) {
    def catchError(using EP: ErrorProcessor[E, A]): IO[A] = {
      eitherT.value.flatMap {
        case Left(err) => EP.processError(err)
        case Right(a)  => a.pure[IO]
      }
    }

    def catchErrorAndDefault(default: => A)(using EP: ErrorProcessor[E, Unit]): IO[A] = {
      eitherT.value.flatMap {
        case Left(err) => EP.processError(err).as(default)
        case Right(a)  => a.pure[IO]
      }
    }

    def catchErrorAndDefaultIO(default: => IO[A])(using EP: ErrorProcessor[E, Unit]): IO[A] = {
      eitherT.value.flatMap {
        case Left(err) => EP.processError(err) *> default
        case Right(a)  => a.pure[IO]
      }
    }
  }
}
