package com.github.chencmd.lootcontainermanager.adapter.database

import cats.effect.kernel.Async

import doobie.*

object SQLite {
  def createTransactor[F[_]: Async](config: DBConfig, debug: Boolean): Transactor[F] = {
    Transactor.fromDriverManager[F](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:${config.filePath.getAbsolutePath()}",
      logHandler = Option.when(debug)(LogHandler.jdkLogHandler)
    )
  }
}
