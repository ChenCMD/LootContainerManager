package com.github.chencmd.lootcontainermanager.minecraft

import cats.effect.SyncIO

trait OnMinecraftThread[F[_]] {
  def run[A](action: SyncIO[A]): F[A]

  def runAndForget[A](action: SyncIO[A]): F[Unit]

  def runLater[A](delay: Long)(action: SyncIO[A]): F[A]

  def runLaterAndForget[A](delay: Long)(action: SyncIO[A]): F[Unit]
}
