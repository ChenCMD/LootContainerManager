package com.github.chencmd.lootcontainermanager.adapter.database

import com.github.chencmd.lootcontainermanager.generic.ThreadExtra

import cats.effect.kernel.Async

import doobie.*
import org.flywaydb.core.Flyway

object SQLite {
  def createTransactor[F[_]: Async](config: DBConfig, debug: Boolean): Transactor[F] = {
    Transactor.fromDriverManager[F](
      driver = "org.sqlite.JDBC",
      url = s"jdbc:sqlite:${config.filePath.getAbsolutePath()}",
      logHandler = Option.when(debug)(LogHandler.jdkLogHandler)
    )
  }

  def migrate[F[_]: Async](plugin: Class[?], config: DBConfig): F[Unit] = Async[F].delay {
    ThreadExtra.withContextClassLoader(plugin.getClassLoader) {
      Flyway
        .configure()
        .dataSource(s"jdbc:sqlite:${config.filePath.getAbsolutePath()}", null, null)
        .baselineOnMigrate(true)
        .baselineVersion("240905")
        .locations("db/migration")
        .load()
        .migrate()
    }
  }
}
