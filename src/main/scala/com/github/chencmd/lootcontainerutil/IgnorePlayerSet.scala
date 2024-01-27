package com.github.chencmd.lootcontainerutil

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.implicits.*

import org.bukkit.entity.Player

class IgnorePlayerSet {
  private val ignorePlayers: Ref[IO, Set[Player]] = Ref.unsafe(Set.empty)

  def registerIgnorePlayer(p: Player): IO[Unit] = ignorePlayers.update(_ + p)

  def removeIgnorePlayer(p: Player): IO[Unit] = ignorePlayers.update(_ - p)

  def isIgnorePlayer(p: Player): IO[Boolean] = ignorePlayers.get.map(_.exists(_.getUniqueId() === p.getUniqueId()))
}
