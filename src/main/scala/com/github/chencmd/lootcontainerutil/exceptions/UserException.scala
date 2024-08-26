package com.github.chencmd.lootcontainermanager.exceptions

import cats.ApplicativeError

class UserException(message: String) extends Exception(message)

object UserException {
  def raise[F[_], A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
    AE.raiseError(new UserException(message))
}
