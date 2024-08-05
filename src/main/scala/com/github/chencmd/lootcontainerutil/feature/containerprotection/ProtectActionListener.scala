package com.github.chencmd.lootcontainerutil.feature.containerprotection

import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

import cats.effect.kernel.Async
import cats.effect.kernel.Sync

import org.bukkit.event.EventHandler
import org.bukkit.event.Listener
import org.bukkit.event.player.PlayerInteractEvent

class ProtectActionListener[F[_]: Async, G[_]: Sync] private (
  private val unsafeRunSyncContinuation: [A] => SyncContinuation[F, G, A] => A
)(using
  mcThread: OnMinecraftThread[F]
) extends Listener {
  @EventHandler def onPlayerInteract(e: PlayerInteractEvent): Unit = {
    val isCancelEvent = unsafeRunSyncContinuation(LootContainerProtection.onPlayerInteract[F, G](e))
    if (isCancelEvent) {
      e.setCancelled(true)
    }
  }
}

object ProtectActionListener {
  def apply[F[_]: Async, G[_]: Sync](unsafeRunSyncContinuation: [A] => SyncContinuation[F, G, A] => A)(using
    mcThread: OnMinecraftThread[F]
  ): F[ProtectActionListener[F, G]] = Async[F].delay {
    new ProtectActionListener[F, G](unsafeRunSyncContinuation)
  }
}
