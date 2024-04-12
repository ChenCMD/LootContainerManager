package com.github.chencmd.lootcontainerutil.exceptions

import cats.ApplicativeError

class SystemException(message: String) extends Exception(message)

object SystemException {
  class RaiseOps[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
      AE.raiseError(new SystemException(message))
  }

  def raise[F[_]] = new RaiseOps[F]
}
