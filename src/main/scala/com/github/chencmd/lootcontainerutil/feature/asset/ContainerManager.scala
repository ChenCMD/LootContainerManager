package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.Prefix
import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession
import com.github.chencmd.lootcontainerutil.terms.InventoriesStore
import com.github.chencmd.lootcontainerutil.terms.InventoriesStore.*

import cats.data.OptionT
import cats.effect.Async
import cats.effect.SyncIO
import cats.effect.kernel.Sync
import cats.implicits.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import org.bukkit.Sound
import org.bukkit.SoundCategory
import org.bukkit.event.block.Action
import org.bukkit.event.inventory.InventoryCloseEvent
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.Inventory

class ContainerManager[F[_]: Async, G[_]: Sync] private (private val openedInventories: InventoriesStore[F])(using
  mcThread: OnMinecraftThread[F],
  asyncLootAssetCache: LootAssetPersistenceCacheInstr[F],
  syncLootAssetCache: LootAssetPersistenceCacheInstr[G],
  itemConverter: ItemConversionInstr[F]
) {
  def onPlayerInteract(e: PlayerInteractEvent): SyncContinuation[F, G, Boolean] = e.getAction match {
    case Action.LEFT_CLICK_BLOCK  => onLeftClickBlock(e)
    case Action.RIGHT_CLICK_BLOCK => onRightClickBlock(e)
    case _                        => Sync[G].pure((false, Async[F].unit))
  }

  def onLeftClickBlock(e: PlayerInteractEvent): SyncContinuation[F, G, Boolean] = {
    val p       = e.getPlayer
    val program = for {
      inv <- OptionT.pure[G](p.getInventory())
      hasItemInHand = !inv.getItemInMainHand.getType.isAir || !inv.getItemInOffHand.getType.isAir
      if !(p.isSneaking() && hasItemInHand)

      loc = BlockLocation.of(e.getClickedBlock().getLocation())
      existsAsset <- OptionT.liftF(syncLootAssetCache.askIfLootAssetPresentAt(loc))
      if existsAsset
    } yield ()

    def effect(): F[Unit] = for {
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.WARNING} Asset の設定されているコンテナです。"))
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.WARNING} Asset を削除する場合は /lcu del_asset を実行してください。"))
      loc = p.getLocation
      _ <- Async[F].delay(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
      _ <- Async[F].sleep(0.08.seconds)
      _ <- Async[F].delay(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
    } yield ()

    program.value.map(v => (v.nonEmpty, v.traverse_(_ => effect())))
  }

  def onRightClickBlock(e: PlayerInteractEvent): SyncContinuation[F, G, Boolean] = {
    val p             = e.getPlayer
    val assetLocation = for {
      inv <- OptionT.pure[G](p.getInventory())
      hasItemInHand = !inv.getItemInMainHand.getType.isAir || !inv.getItemInOffHand.getType.isAir
      if !(p.isSneaking() && hasItemInHand)

      loc = BlockLocation.of(e.getClickedBlock().getLocation())
      existsAsset <- OptionT.liftF(syncLootAssetCache.askIfLootAssetPresentAt(loc))
      if existsAsset

      _ <- OptionT.liftF { Sync[G].delay { p.closeInventory() } }
    } yield loc

    def effect(assetLocation: BlockLocation): F[Unit] = for {
      asset <- asyncLootAssetCache.askLootAssetLocationAt(assetLocation)
      asset <- asset.fold(SystemException.raise[F]("Asset not found"))(_.pure[F])
      inv   <- openedInventories.getOrCreateInventory(assetLocation, asset)
      _     <- mcThread.run(SyncIO(p.openInventory(inv.getInventory())))
    } yield ()

    assetLocation.value.map(loc => (loc.nonEmpty, loc.traverse_(effect)))
  }

  def onInventoryClose(e: InventoryCloseEvent): SyncContinuation[F, G, Unit] = {
    val program = for {
      inv <- OptionT.liftF(Sync[G].delay(e.getInventory()))
      if inv.getViewers().asScala.sizeIs == 1

      holder <- OptionT.fromOption(inv.getHolder().downcastOrNone[InventorySession])
      if holder.id == ContainerManager.INVENTORY_NAME
    } yield (inv, holder)

    def effect(inv: Inventory, holder: InventorySession): F[Unit] = {
      openedInventories.withLockAtKey(holder.location) { _ =>
        for {
          asset <- asyncLootAssetCache.askLootAssetLocationAt(holder.location)
          asset <- asset.fold(SystemException.raise[F]("Asset not found"))(_.pure[F])
          items <- GenLootAsset.convertToLootAssetItem(inv)
          hasChanged = asset.items.zip(items).exists((a, b) => a != b)

          _ <- Async[F].whenA(hasChanged)(asyncLootAssetCache.updateLootAsset(asset.copy(items = items)))
        } yield (None, ())
      }
    }

    program.value.map(v => (v.nonEmpty, v.traverse_(effect.tupled)))
  }
}

object ContainerManager {
  val INVENTORY_NAME = "LOOT_ASSET_CONTAINER"

  def apply[F[_]: Async, G[_]: Sync](openedInventories: InventoriesStore[F])(using
    mcThread: OnMinecraftThread[F],
    syncLootAssetCache: LootAssetPersistenceCacheInstr[G],
    asyncLootAssetCache: LootAssetPersistenceCacheInstr[F],
    itemConverter: ItemConversionInstr[F]
  ): F[ContainerManager[F, G]] = Async[F].delay {
    new ContainerManager[F, G](openedInventories)
  }
}
