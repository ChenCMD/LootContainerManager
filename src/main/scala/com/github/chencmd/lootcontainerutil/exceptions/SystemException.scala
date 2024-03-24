package com.github.chencmd.lootcontainerutil.exceptions

import cats.ApplicativeError

class SystemException(message: String) extends Exception(message)

object SystemException {
  def raise[F[_], A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
    AE.raiseError(new SystemException(message))
}
