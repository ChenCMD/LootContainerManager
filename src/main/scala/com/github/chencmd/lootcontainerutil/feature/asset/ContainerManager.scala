package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.Prefix
import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.KeyedMutex
import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession

import cats.data.OptionT
import cats.effect.Async
import cats.effect.SyncIO
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.implicits.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import org.bukkit.Bukkit
import org.bukkit.Sound
import org.bukkit.SoundCategory
import org.bukkit.event.block.Action
import org.bukkit.event.inventory.InventoryCloseEvent
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.Inventory

class ContainerManager[F[_]: Async, G[_]: Sync] private (
  private val openedInventories: KeyedMutex[F, BlockLocation, InventorySession]
)(using
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
      inv   <- getOrCreateInventory(assetLocation, asset)
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

  def getOrCreateInventory(location: BlockLocation, asset: LootAsset): F[InventorySession] = {
    def createInventorySession = for {
      items   <- asset.items.traverse(i => itemConverter.toItemStack(i.item).map((i.slot, _, i.quantity)))
      session <- InventorySession[F](ContainerManager.INVENTORY_NAME, location) { holder =>
        val server = Bukkit.getServer()
        asset.name.fold(server.createInventory(holder, 27))(server.createInventory(holder, 27, _)).tap { inv =>
          items.foreach { (slot, item, quantity) =>
            item.setAmount(quantity)
            inv.setItem(slot, item)
          }
        }
      }
    } yield session

    openedInventories.withLockAtKey(location) { invOrNone =>
      invOrNone.fold(createInventorySession)(_.pure[F]).map(inv => (Some(inv), inv))
    }
  }
}

object ContainerManager {
  val INVENTORY_NAME = "LOOT_ASSET_CONTAINER"

  def apply[F[_]: Async, G[_]: Sync](openedInventoriesRef: Ref[F, Map[BlockLocation, InventorySession]])(using
    mcThread: OnMinecraftThread[F],
    syncLootAssetCache: LootAssetPersistenceCacheInstr[G],
    asyncLootAssetCache: LootAssetPersistenceCacheInstr[F],
    itemConverter: ItemConversionInstr[F]
  ): F[ContainerManager[F, G]] = for {
    keyedMutex <- KeyedMutex.empty[F, BlockLocation, InventorySession]
    cm         <- Async[F].delay(new ContainerManager[F, G](keyedMutex))
  } yield cm
}
