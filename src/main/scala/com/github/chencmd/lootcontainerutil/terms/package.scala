package com.github.chencmd.lootcontainermanager

import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.feature.asset.ContainerManager
import com.github.chencmd.lootcontainermanager.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainermanager.generic.KeyedMutex
import com.github.chencmd.lootcontainermanager.minecraft.ContainerMeta
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.InventorySession

import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits.*
import org.typelevel.log4cats.Logger

import java.util.UUID

import org.bukkit.Bukkit

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
      def getOrCreateInventory[G[_]: Sync](location: BlockLocation, asset: LootAsset.Fixed, debug: Boolean)(using
        logger: Logger[F],
        mcThread: OnMinecraftThread[F, G],
        itemConverter: ItemConversionInstr[F, G]
      ): F[(Boolean, InventorySession)] = {
        def createInventorySession: F[InventorySession] = for {
          (generatedTime, items) <- Async[F].timed(mcThread.run {
            asset.items.traverse(i => itemConverter.toItemStack(i.item).map((i.slot, _, i.quantity)))
          })
          blockId       = asset.containers.head.blockId
          containerMeta = ContainerMeta.fromId(blockId.drop("minecraft:".length))
          containerMeta <- containerMeta.fold(SystemException.raise[F](s"Unknown container type: $blockId"))(_.pure[F])

          session <- InventorySession[F](ContainerManager.INVENTORY_NAME, location, containerMeta) { holder =>
            val server = Bukkit.getServer()
            val inv    = containerMeta match {
              case ContainerMeta.Chest => asset.name match {
                  case Some(name) => server.createInventory(holder, 27 * asset.containers.length, name)
                  case None       => server.createInventory(holder, 27 * asset.containers.length)
                }
              case _                   => asset.name match {
                  case Some(name) => server.createInventory(holder, containerMeta.inventoryType, name)
                  case None       => server.createInventory(holder, containerMeta.inventoryType)
                }
            }
            items.foreach { (slot, item, quantity) =>
              item.setAmount(quantity)
              inv.setItem(slot, item)
            }
            inv
          }

          _ <- Async[F].whenA(debug)(logger.info(s"Generated inventory in ${generatedTime.toMillis}ms."))
        } yield session

        openedInventories.withLockAtKey(location) { invOrNone =>
          invOrNone.fold(createInventorySession)(_.pure[F]).map(i => (Some(i), (invOrNone.isDefined, i)))
        }
      }
    }
  }
}
