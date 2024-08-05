package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession

import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.implicits.*

import org.bukkit.event.EventHandler
import org.bukkit.event.Listener
import org.bukkit.event.inventory.InventoryCloseEvent
import org.bukkit.event.player.PlayerInteractEvent

class ContainerManageListener[F[_]: Async, G[_]: Sync] private (
  private val cm: ContainerManager[F, G],
  private val unsafeRunSyncContinuation: [A] => SyncContinuation[F, G, A] => A
) extends Listener {
  @EventHandler def onPlayerInteract(e: PlayerInteractEvent): Unit = {
    val isCancelEvent = unsafeRunSyncContinuation(cm.onPlayerInteract(e))
    if (isCancelEvent) {
      e.setCancelled(true)
    }
  }

  @EventHandler def onInventoryClose(e: InventoryCloseEvent): Unit = {
    unsafeRunSyncContinuation(cm.onInventoryClose(e))
  }
}

object ContainerManageListener {
  def apply[F[_]: Async, G[_]: Sync](openedInventoriesRef: Ref[F, Map[BlockLocation, InventorySession]])(
    unsafeRunSyncContinuation: [A] => SyncContinuation[F, G, A] => A
  )(using
    mcThread: OnMinecraftThread[F],
    AsyncLAPCI: LootAssetPersistenceCacheInstr[F],
    SyncLAPCI: LootAssetPersistenceCacheInstr[G],
    Converter: ItemConversionInstr[F],
    LAP: LootAssetPersistenceInstr[F]
  ): F[ContainerManageListener[F, G]] = for {
    cm  <- ContainerManager[F, G](openedInventoriesRef)
    cml <- Async[F].delay(new ContainerManageListener[F, G](cm, unsafeRunSyncContinuation))
  } yield cml
}
