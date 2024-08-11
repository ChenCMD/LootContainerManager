package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import com.github.tarao.record4s.%
import com.github.tarao.record4s.unselect
import org.bukkit.block.Container
import org.bukkit.block.data.Directional
import org.bukkit.block.data.Waterlogged
import org.bukkit.block.data.`type`.Chest
import org.bukkit.entity.Player
import org.bukkit.inventory.Inventory

object GenLootAsset {
  type ContainerData = % {
    val location: BlockLocation
    val blockId: String
    val name: Option[String]
    val facing: Option[org.bukkit.block.BlockFace]
    val waterlogged: Option[Boolean]
    val chestType: Option[Chest.Type]
    val inventory: Inventory
  }

  def generateLootAsset[F[_]: Async](p: Player)(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ): F[Unit] = for {
    // プレイヤーが見ているコンテナの情報を取得する
    dataOrNone <- mcThread.run(for {
      containerOrNone <- findContainer(p)
      dataOrNone      <- containerOrNone.traverse(getContainerData)
    } yield dataOrNone)
    data       <- dataOrNone.fold(UserException.raise("No container was found."))(_.pure[F])

    // コンテナを見ているプレイヤーが居たら閉じる
    _ <- closeContainer(data.inventory)

    // コンテナの情報を取得する
    items <- convertToLootAssetItem(data.inventory)
    asset = convertToLootAsset(data, items)

    // LootAsset を保存する
    _ <- LAPCI.updateLootAsset(asset)

    // プレイヤーにメッセージを送信する
    _ <- Async[F].delay(p.sendMessage(s"Generated loot asset at ${asset.location}"))
  } yield ()

  def findContainer(p: Player): SyncIO[Option[Container]] = SyncIO {
    Option(p.getTargetBlockExact(5))
      .flatMap(_.getState.downcastOrNone[Container])
      .headOption
  }

  def getContainerData(container: Container): SyncIO[ContainerData] = SyncIO {
    val data = container.getBlockData()
    %(
      "location"    -> BlockLocation.of(container.getLocation()),
      "blockId"     -> container.getType().getKey().toString(),
      "name"        -> Option(container.getCustomName()),
      "facing"      -> data.downcastOrNone[Directional].map(_.getFacing),
      "waterlogged" -> data.downcastOrNone[Waterlogged].map(_.isWaterlogged),
      "chestType"   -> data.downcastOrNone[Chest].map(_.getType),
      "inventory"   -> container.getInventory()
    )
  }

  def closeContainer[F[_]: Async](inv: Inventory): F[Unit] = {
    inv.getViewers().asScala.toList.traverse_(p => Async[F].delay(p.closeInventory()))
  }

  def convertToLootAssetItem[F[_]: Async](
    inv: Inventory
  )(using Converter: ItemConversionInstr[F]): F[List[LootAssetItem]] = {
    inv
      .getContents()
      .toList
      .map(Option.apply)
      .zipWithIndex
      .traverseCollect {
        case (Some(item), slot) => Converter.toItemIdentifier(item).map(LootAssetItem(slot, _, item.getAmount()))
      }
  }

  def convertToLootAsset[F[_]: Async](data: ContainerData, items: List[LootAssetItem]): LootAsset = {
    (%("id" -> None) ++ data(unselect.inventory) ++ %("items" -> items)).to[LootAsset]
  }
}
