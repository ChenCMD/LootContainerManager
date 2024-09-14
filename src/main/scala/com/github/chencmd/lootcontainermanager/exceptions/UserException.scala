package com.github.chencmd.lootcontainermanager.exceptions

import cats.ApplicativeError

class UserException(message: String) extends Exception(message)

object UserException {
  class RaiseOps[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
      AE.raiseError(new UserException(message))
  }

  def raise[F[_]] = new RaiseOps[F]
}
