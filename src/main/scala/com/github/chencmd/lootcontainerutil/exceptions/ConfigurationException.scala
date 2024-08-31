package com.github.chencmd.lootcontainermanager.exceptions

import cats.ApplicativeError

class ConfigurationException(message: String) extends Exception(message)

object ConfigurationException {
  class RaiseOps[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
      AE.raiseError(new ConfigurationException(message))
  }
  def raise[F[_]] = new RaiseOps[F]
}
