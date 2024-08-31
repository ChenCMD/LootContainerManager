package com.github.chencmd.lootcontainermanager.minecraft

trait OnMinecraftThread[F[_], G[_]] {
  def run[A](action: G[A]): F[A]

  def runAndForget[A](action: G[A]): F[Unit]

  def runLater[A](delay: Long)(action: G[A]): F[A]

  def runLaterAndForget[A](delay: Long)(action: G[A]): F[Unit]
}
