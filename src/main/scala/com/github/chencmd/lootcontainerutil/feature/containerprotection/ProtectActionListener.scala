package com.github.chencmd.lootcontainerutil.feature.containerprotection

import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import org.bukkit.event.EventHandler
import org.bukkit.event.Listener
import org.bukkit.event.player.PlayerInteractEvent

class ProtectActionListener(using mcThread: OnMinecraftThread[IO]) extends Listener {
  type F = IO[_]

  @EventHandler def onPlayerInteract(e: PlayerInteractEvent): Unit = {
    val (isCancelEvent, effect) = LootContainerProtection.onPlayerInteract[F](e).unsafeRunSync()
    if (isCancelEvent) {
      e.setCancelled(true)
    }
    effect.unsafeRunAndForget()
  }
}
