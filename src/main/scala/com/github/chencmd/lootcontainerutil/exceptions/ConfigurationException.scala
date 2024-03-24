package com.github.chencmd.lootcontainerutil.exceptions

import cats.ApplicativeError

class ConfigurationException(message: String) extends Exception(message)

object ConfigurationException {
  def raise[F[_], A](message: String)(using AE: ApplicativeError[F, Throwable]): F[A] =
    AE.raiseError(new ConfigurationException(message))
}
