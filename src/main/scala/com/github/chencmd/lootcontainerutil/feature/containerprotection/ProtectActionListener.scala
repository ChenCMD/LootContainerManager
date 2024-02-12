package com.github.chencmd.lootcontainerutil.feature.containerprotection

import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.generic.EitherTIOExtra.*
import com.github.chencmd.lootcontainerutil.utils.CommonErrorHandler.given

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import org.bukkit.event.EventHandler
import org.bukkit.event.Listener
import org.bukkit.event.player.PlayerInteractEvent
import cats.data.EitherT

class ProtectActionListener(using mcThread: OnMinecraftThread[EitherT[IO, String, _]]) extends Listener {
  @EventHandler def onPlayerInteract(e: PlayerInteractEvent): Unit = {
    val (isCancelEvent, effect) = LootContainerProtection.onPlayerInteract[EitherT[IO, String, _]](e).unsafeRunSync()
    if (isCancelEvent) {
      e.setCancelled(true)
    }
    effect.catchError.unsafeRunAndForget()
  }
}
