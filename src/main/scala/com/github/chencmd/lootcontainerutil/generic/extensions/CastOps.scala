package com.github.chencmd.lootcontainerutil.generic.extensions

import cats.implicits.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import cats.mtl.Raise
import cats.data.EitherNec
import cats.Applicative

object CastOps {
  final class DowncastOrRaiseOps[A, B](val value: A) extends AnyVal {
    def apply[F[_]: Applicative]()(using R: Raise[F, String], tt: TypeTest[A, B], ctA: ClassTag[A], ctB: ClassTag[B]): F[A & B] = value match {
      case tt(b) => b.pure[F]
      case _     => R.raise(s"Downcast failed: $ctA to $ctB")
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

    def downcastOrLeft[B](using tt: TypeTest[A, B], ctA: ClassTag[A], ctB: ClassTag[B]): Either[String, A & B] = value match {
      case tt(b) => Right(b)
      case _     => Left(s"Downcast failed: $ctA to $ctB")
    }

    def downcastOrLeftNec[B](using tt: TypeTest[A, B], ctA: ClassTag[A], ctB: ClassTag[B]): EitherNec[String, A & B] = value match {
      case tt(b) => Right(b)
      case _     => Either.leftNec(s"Downcast failed: $ctA to $ctB")
    }

    def downcastOrElse[B] = new DowncastOrElseOps[A, B](value)

    def downcastOrElseM[B] = new DowncastOrElseMOps[A, B](value)

    def downcastOrRaise[B] = new DowncastOrRaiseOps[A, B](value)
  }
}
