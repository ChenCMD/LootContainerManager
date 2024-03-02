package com.github.chencmd.lootcontainerutil.adapter.database

import cats.effect.kernel.Async

import doobie.*

object SQLite {
  def createTransactor[F[_]: Async](config: DBConfig): Transactor[F] = {
    Transactor.fromDriverManager[F](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:${config.filePath.getAbsolutePath()}",
      user = config.user,
      password = config.password,
      logHandler = Some(LogHandler.jdkLogHandler)
    )
  }
}
