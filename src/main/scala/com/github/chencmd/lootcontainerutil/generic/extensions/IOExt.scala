package com.github.chencmd.lootcontainermanager.generic.extensions

import cats.effect.IO
import cats.effect.unsafe.IORuntime

object IOExt {
  extension [A](io: IO[A]) {
    def unsafeRunHereSync()(using runtime: IORuntime): A = {
      io.syncStep(Int.MaxValue).unsafeRunSync() match {
        case Left(v)  => v.unsafeRunSync()
        case Right(v) => v
      }
    }
  }
}
