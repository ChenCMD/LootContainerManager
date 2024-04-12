package com.github.chencmd.lootcontainerutil.generic.extensions

import cats.Applicative
import cats.ApplicativeError
import cats.data.EitherNec
import cats.implicits.*

import scala.reflect.ClassTag
import scala.reflect.TypeTest

object CastOps {
  final class DowncastOrLeftOps[A, B](val value: A) extends AnyVal {
    def apply[E](onFailed: => E)(using tt: TypeTest[A, B]): Either[E, A & B] = value match {
      case tt(b) => Right(b)
      case _     => Left(onFailed)
    }
  }

  final class DowncastOrLeftNecOps[A, B](val value: A) extends AnyVal {
    def apply[E](onFailed: => E)(using tt: TypeTest[A, B]): EitherNec[E, A & B] = value match {
      case tt(b) => Right(b)
      case _     => Either.leftNec(onFailed)
    }
  }

  final class DowncastOrRaiseOps[A, B](val value: A) extends AnyVal {
    def apply[F[_]: Applicative]()(using
      tt: TypeTest[A, B],
      ctA: ClassTag[A],
      ctB: ClassTag[B],
      AE: ApplicativeError[F, Throwable]
    ): F[A & B] = value match {
      case tt(b) => b.pure[F]
      case _     => AE.raiseError(new ClassCastException(s"Downcast failed: $ctA to $ctB"))
    }
  }

  final class DowncastOrElseOps[A, B](val value: A) extends AnyVal {
    def apply[C >: A & B](onFailed: => C)(using tt: TypeTest[A, B]): C = value match {
      case tt(b) => b
      case _     => onFailed
    }
  }

  final class DowncastOrElseMOps[A, B](val value: A) extends AnyVal {
    def apply[F[_]: Applicative, C >: A & B](onFailed: => F[C])(using tt: TypeTest[A, B]): F[C] = value match {
      case tt(b) => b.pure[F]
      case _     => onFailed
    }
  }

  extension [A](value: A) {
    def downcastOrNone[B](using tt: TypeTest[A, B]): Option[A & B] = value match {
      case tt(b) => Some(b)
      case _     => None
    }

    def downcastOrLeft[B](using tt: TypeTest[A, B], ctA: ClassTag[A], ctB: ClassTag[B]): Either[String, A & B] =
      value match {
        case tt(b) => Right(b)
        case _     => Left(s"Downcast failed: $ctA to $ctB")
      }

    def downcastOrLeftNec[B](using tt: TypeTest[A, B], ctA: ClassTag[A], ctB: ClassTag[B]): EitherNec[String, A & B] =
      value match {
        case tt(b) => Right(b)
        case _     => Either.leftNec(s"Downcast failed: $ctA to $ctB")
      }

    def downcastOrLeft[B] = new DowncastOrLeftOps[A, B](value)

    def downcastOrLeftNec[B] = new DowncastOrLeftNecOps[A, B](value)

    def downcastOrElse[B] = new DowncastOrElseOps[A, B](value)

    def downcastOrElseM[B] = new DowncastOrElseMOps[A, B](value)

    def downcastOrRaise[B] = new DowncastOrRaiseOps[A, B](value)
  }
}
