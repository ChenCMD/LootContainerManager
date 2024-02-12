package com.github.chencmd.lootcontainerutil.utils

import com.github.chencmd.lootcontainerutil.generic.EitherTIOExtra.ErrorProcessor
import cats.effect.IO
import org.bukkit.Bukkit
import cats.data.NonEmptyChain
import cats.implicits.*
import cats.effect.SyncIO

object CommonErrorHandler {
  given ErrorProcessor[String, Unit] with                {
    def processError(err: String): IO[Unit] = SyncIO(Bukkit.getConsoleSender.sendMessage(err)).to[IO]
  }

  given ErrorProcessor[NonEmptyChain[String], Unit] with {
    def processError(err: NonEmptyChain[String]): IO[Unit] =
      SyncIO(Bukkit.getConsoleSender.sendMessage(err.mkString_("", "\n", ""))).to[IO]
  }
}
