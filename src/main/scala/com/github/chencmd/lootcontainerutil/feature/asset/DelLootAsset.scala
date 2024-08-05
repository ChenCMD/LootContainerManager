package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession

import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.implicits.*

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import org.bukkit.Bukkit
import org.bukkit.block.Container
import org.bukkit.entity.Player

object DelLootAsset {
  def deleteLootAsset[F[_]: Async](p: Player, openedInventories: Ref[F, Map[BlockLocation, InventorySession]])(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    asyncLootAssetCache: LootAssetPersistenceCacheInstr[F]
  ): F[Unit] = for {
    container <- mcThread.run(for {
      blockOrNone     <- SyncIO(Option(p.getTargetBlockExact(5)))
      containerOrNone <- SyncIO(blockOrNone.flatMap(_.getState.downcastOrNone[Container]))
    } yield containerOrNone)
    container <- container.fold(UserException.raise("No container was found"))(_.pure[F])

    assetLocation = BlockLocation.of(container.getLocation())

    asset <- asyncLootAssetCache.askLootAssetLocationAt(assetLocation)
    asset <- asset.fold(UserException.raise("Asset not found"))(_.pure[F])

    assetSession <- getOrCreateInventory(openedInventories)(assetLocation, asset)
    assetInv = assetSession.getInventory()

    _ <- Async[F].delay { assetInv.getViewers().asScala.foreach(_.closeInventory()) }

    _ <- Async[F].delay {
      val containerInv = container.getInventory()
      containerInv.clear()
      containerInv.setContents(assetInv.getContents())
    }

    _ <- openedInventories.update(_ - assetLocation)
    _ <- asyncLootAssetCache.deleteLootAssetLocationAt(assetLocation)

    _ <- Async[F].delay { p.sendMessage("Asset removed") }
  } yield ()

  // ContainerManager.scala の getOrCreateInventory とほぼ同じなので共通化する
  def getOrCreateInventory[F[_]: Async](openedInventories: Ref[F, Map[BlockLocation, InventorySession]])(
    location: BlockLocation,
    asset: LootAsset
  )(using itemConverter: ItemConversionInstr[F]): F[InventorySession] = {
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
      _       <- openedInventories.update(_ + (location -> session))
    } yield session

    for {
      map <- openedInventories.get
      inv <- map.get(location).map(_.pure[F]).getOrElse(createInventorySession)
    } yield inv
  }
}
