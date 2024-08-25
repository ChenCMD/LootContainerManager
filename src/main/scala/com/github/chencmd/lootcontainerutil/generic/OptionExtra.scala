package com.github.chencmd.lootcontainerutil.generic

import cats.Applicative
import cats.implicits.*

object OptionExtra {
  extension [A](optionA: Option[A]) {
    def getOrElseEffectfully[F[_]: Applicative](fa: F[A]): F[A] = {
      optionA.fold(fa)(_.pure[F])
    }
  }
}
