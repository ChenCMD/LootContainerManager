package com.github.chencmd.lootcontainermanager.feature.asset

import com.github.chencmd.lootcontainermanager.Prefix
import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainermanager.generic.SyncContinuation
import com.github.chencmd.lootcontainermanager.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.InventorySession
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.Vector
import com.github.chencmd.lootcontainermanager.terms.InventoriesStore
import com.github.chencmd.lootcontainermanager.terms.InventoriesStore.*

import cats.data.OptionT
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.implicits.*
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import org.bukkit.Sound
import org.bukkit.SoundCategory
import org.bukkit.block.Chest
import org.bukkit.block.data.`type`.Chest as ChestData
import org.bukkit.entity.Player
import org.bukkit.event.block.Action
import org.bukkit.event.block.BlockPlaceEvent
import org.bukkit.event.inventory.InventoryCloseEvent
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.Inventory

class ContainerManager[F[_]: Async, G[_]: Sync] private (
  private val openedInventories: InventoriesStore[F],
  private val debug: Boolean
)(using
  logger: Logger[F],
  mcThread: OnMinecraftThread[F, G],
  asyncLootAssetCache: LootAssetPersistenceCacheInstr[F],
  syncLootAssetCache: LootAssetPersistenceCacheInstr[G],
  itemConverter: ItemConversionInstr[F, G]
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
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.WARNING} Asset を削除する場合は /lcm del_asset を実行してください。"))
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
      asset <- asset.fold(SystemException.raise[F]("Asset not found"))(_.downcastOrNone[LootAsset.Fixed].pure[F])
      _     <- asset.traverse_ { asset =>
        for {
          (created, inv) <- openedInventories.getOrCreateInventory[G](assetLocation, asset, debug)
          _              <- mcThread.run(Sync[G].delay(p.openInventory(inv.getInventory())))
          _              <- inv.containerMeta.openedSound.traverse_ { se =>
            Async[F].delay(p.getWorld.playSound(p.getLocation, se, SoundCategory.BLOCKS, 1f, 1))
          }
        } yield ()
      }
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

    def effect(inv: Inventory, holder: InventorySession): F[Unit] = for {
      _ <- openedInventories.withLockAtKey(holder.location) { _ =>
        for {
          asset <- asyncLootAssetCache.askLootAssetLocationAt(holder.location)
          asset <- asset.fold(SystemException.raise[F]("Asset not found"))(_.pure[F])
          asset <- asset.downcastOrRaise[LootAsset.Fixed][F]()
          items <- GenLootAsset.convertToLootAssetItem[F, G](inv)
          hasChanged = !asset.items.sameElements(items)

          _ <- Async[F].whenA(hasChanged)(for {
            _ <- asyncLootAssetCache.updateLootAsset(asset.copy(items = items))
            _ <- Async[F].whenA(debug)(logger.info(s"Asset modified at ${holder.location.toXYZString}"))
          } yield ())
        } yield (None, ())
      }
      _ <- holder.containerMeta.closedSound.traverse_ { se =>
        Async[F].delay {
          val loc = e.getPlayer().getLocation
          loc.getWorld.playSound(loc, se, SoundCategory.BLOCKS, 1f, 1)
        }
      }
    } yield ()

    program.value.map(v => (v.nonEmpty, v.traverse_(effect.tupled)))
  }

  def onBlockPlaceEvent(e: BlockPlaceEvent): SyncContinuation[F, G, Boolean] = {
    val program = for {
      chest <- OptionT.fromOption[G](e.getBlockPlaced.getState.downcastOrNone[Chest])
      data  <- OptionT.fromOption[G](chest.getBlockData.downcastOrNone[ChestData])
      location  = BlockLocation.of(chest.getLocation())
      chestType = data.getType
      if chestType != ChestData.Type.SINGLE
      facing    = data.getFacing

      rotation <- OptionT.fromOption[G](chestType match {
        case ChestData.Type.SINGLE => None
        case ChestData.Type.LEFT   => Some(+90)
        case ChestData.Type.RIGHT  => Some(-90)
      })
      vector = Vector.of(facing.getDirection).rotate(rotation)

      existsAsset <- OptionT.liftF(syncLootAssetCache.askIfLootAssetPresentAt((location + vector).toBlockLocation))
      if existsAsset
    } yield ()

    def effect(p: Player): F[Unit] = for {
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.WARNING} Asset の設定されているコンテナをラージチェストに変更することはできません。"))
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.WARNING} Asset を拡張する場合は /lcm del_asset で削除してから再度設定してください。"))
    } yield ()

    program.value.map(v => (v.nonEmpty, v.traverse_(_ => effect(e.getPlayer))))
  }
}

object ContainerManager {
  val INVENTORY_NAME = "LOOT_ASSET_CONTAINER"

  def apply[F[_]: Async, G[_]: Sync](openedInventories: InventoriesStore[F], debug: Boolean)(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F, G],
    syncLootAssetCache: LootAssetPersistenceCacheInstr[G],
    asyncLootAssetCache: LootAssetPersistenceCacheInstr[F],
    itemConverter: ItemConversionInstr[F, G]
  ): F[ContainerManager[F, G]] = Async[F].delay {
    new ContainerManager[F, G](openedInventories, debug)
  }
}
