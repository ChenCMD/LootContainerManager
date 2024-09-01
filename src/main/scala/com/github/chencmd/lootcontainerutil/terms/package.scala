package com.github.chencmd.lootcontainermanager

import com.github.chencmd.lootcontainermanager.feature.asset.ContainerManager
import com.github.chencmd.lootcontainermanager.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainermanager.generic.KeyedMutex
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.InventorySession

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits.*

import java.util.UUID

import org.bukkit.Bukkit
import org.bukkit.event.inventory.InventoryType
import org.bukkit.inventory.Inventory
import org.bukkit.inventory.InventoryHolder
import org.bukkit.inventory.ItemStack
import org.typelevel.log4cats.Logger

package object terms {
  case class LootAssetCache(
    assets: Map[BlockLocation, Map[BlockLocation, UUID]],
    mapping: Map[UUID, LootAsset],
    updatedAssetLocations: Set[UUID],
    deletedAssetIds: Set[Int]
  ) {
    def askLootAssetLocationAt(location: BlockLocation): Option[LootAsset] = for {
      forChunk <- assets.get(location.toChunkLocation)
      uuid     <- forChunk.get(location)
      asset    <- mapping.get(uuid)
    } yield asset
  }

  object LootAssetCache {
    def empty: LootAssetCache = LootAssetCache(Map.empty, Map.empty, Set.empty, Set.empty)
  }

  type InventoriesStore[F[_]] = KeyedMutex[F, BlockLocation, InventorySession]

  object InventoriesStore {
    def empty[F[_]: Async]: F[InventoriesStore[F]] = KeyedMutex.empty[F, BlockLocation, InventorySession]

    extension [F[_]: Async](openedInventories: InventoriesStore[F]) {
      def getOrCreateInventory[G[_]: Sync](location: BlockLocation, asset: LootAsset)(using
        logger: Logger[F],
        mcThread: OnMinecraftThread[F, G],
        itemConverter: ItemConversionInstr[F, G]
      ): F[InventorySession] = {
        def createInventory(items: List[(Int, ItemStack, Int)])(holder: InventoryHolder): Inventory = {
          val server = Bukkit.getServer()
          val inv    = {
            if (asset.containers.size > 1) {
              val size = asset.containers.size * 27
              asset.name.fold(server.createInventory(holder, size))(server.createInventory(holder, size, _))
            } else {
              val invType = asset.containers.head.blockId.drop("minecraft:".length) match {
                case "brewing_stand" => InventoryType.BREWING
                case id              => InventoryType.valueOf(id.toUpperCase())
              }
              asset.name.fold(server.createInventory(holder, invType))(server.createInventory(holder, invType, _))
            }
          }
          items.foreach { (slot, item, quantity) =>
            item.setAmount(quantity)
            inv.setItem(slot, item)
          }
          inv
        }

        def createInventorySession: F[InventorySession] = for {
          (generatedTime, items) <- Async[F].timed(mcThread.run {
            asset.items.traverse(i => itemConverter.toItemStack(i.item).map((i.slot, _, i.quantity)))
          })
          _                      <- logger.info(s"Generated inventory in ${generatedTime.toMillis}ms.")
          session <- InventorySession[F](ContainerManager.INVENTORY_NAME, location)(createInventory(items))
        } yield session

        openedInventories.withLockAtKey(location) { invOrNone =>
          invOrNone.fold(createInventorySession)(_.pure[F]).map(i => (Some(i), i))
        }
      }
    }
  }
}
