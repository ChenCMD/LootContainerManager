package com.github.chencmd.lootcontainermanager.generic.extensions

import cats.Applicative
import cats.implicits.*

object OptionExt {
  extension [A](optionA: Option[A]) {
    def getOrElseEffectfully[F[_]: Applicative](fa: F[A]): F[A] = optionA.fold(fa)(_.pure[F])
  }
}
