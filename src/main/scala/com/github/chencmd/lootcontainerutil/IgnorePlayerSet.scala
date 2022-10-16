package com.github.chencmd.lootcontainerutil

import cats.implicits.*
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, SyncIO}
import generic.givens.PlayerCompare.given
import minecraft.OnMinecraftThread
import org.bukkit.entity.Player

class IgnorePlayerSet(using mcThread: OnMinecraftThread[IO]) {
  private val ignorePlayers: Ref[IO, Set[Player]] = Ref.unsafe(Set.empty)

  def registerIgnorePlayer(p: Player): IO[Unit] = {
    for {
      _ <- ignorePlayers.update(_ + p)
      _ <- mcThread.runLaterAndForget(8 * 20)(ignorePlayers.update(_ - p).syncStep)
    } yield ()
  }

  def removeIgnorePlayer(p: Player): IO[Unit] =
    ignorePlayers.update(_ - p)

  def isIgnorePlayer(p: Player): IO[Boolean] =
    ignorePlayers.get.map(_.exists(_ === p))
}
