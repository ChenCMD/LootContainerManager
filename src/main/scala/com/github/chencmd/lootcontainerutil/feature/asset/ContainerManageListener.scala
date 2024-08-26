package com.github.chencmd.lootcontainermanager.feature.asset

import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainermanager.generic.SyncContinuation
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.terms.InventoriesStore

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits.*
import org.typelevel.log4cats.Logger

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
  def apply[F[_]: Async, G[_]: Sync](openedInventories: InventoriesStore[F])(
    unsafeRunSyncContinuation: [A] => SyncContinuation[F, G, A] => A
  )(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F],
    AsyncLAPCI: LootAssetPersistenceCacheInstr[F],
    SyncLAPCI: LootAssetPersistenceCacheInstr[G],
    Converter: ItemConversionInstr[F],
    LAP: LootAssetPersistenceInstr[F]
  ): F[ContainerManageListener[F, G]] = for {
    cm  <- ContainerManager[F, G](openedInventories)
    cml <- Async[F].delay(new ContainerManageListener[F, G](cm, unsafeRunSyncContinuation))
  } yield cml
}
