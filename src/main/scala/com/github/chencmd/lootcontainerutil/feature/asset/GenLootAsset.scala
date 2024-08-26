package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetContainer
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.Vector
import com.github.chencmd.lootcontainerutil.Prefix

import cats.data.OptionT
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.std.UUIDGen
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import com.github.tarao.record4s.%
import com.github.tarao.record4s.unselect
import org.bukkit.World
import org.bukkit.block.Chest
import org.bukkit.block.Container
import org.bukkit.block.data.Directional
import org.bukkit.block.data.Waterlogged
import org.bukkit.block.data.`type`.Chest as ChestData
import org.bukkit.entity.Player
import org.bukkit.inventory.Inventory

object GenLootAsset {
  type ContainerData = % {
    val location: BlockLocation
    val blockId: String
    val name: Option[String]
    val facing: Option[org.bukkit.block.BlockFace]
    val waterlogged: Option[Boolean]
    val chestType: Option[ChestData.Type]
    val inventory: Inventory
  }

  def generateLootAsset[F[_]: Async](p: Player)(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ): F[Unit] = for {
    // プレイヤーが見ているコンテナの情報を取得する
    containerDataOrNone <- mcThread.run {
      val program = for {
        container     <- OptionT(findContainer(p))
        containerData <- OptionT.liftF(getContainerData(container))

        connectedContainer     <- OptionT.liftF(getConnectedContainer(p.getWorld(), containerData))
        connectedContainerData <- OptionT.liftF(connectedContainer.traverse(getContainerData))
      } yield (containerData, connectedContainerData)
      program.value
    }
    (data, connected)   <- containerDataOrNone.fold(UserException.raise("No container was found."))(_.pure[F])

    // コンテナを見ているプレイヤーが居たら閉じる
    _ <- closeContainer(data.inventory)

    // コンテナの情報を取得する
    items <- convertToLootAssetItem(data.inventory)
    asset <- convertToLootAsset(List(data) ++ connected.toList, items)

    // LootAsset を保存する
    _ <- LAPCI.updateLootAsset(asset)

    // プレイヤーにメッセージを送信する
    _ <- Async[F].delay {
      p.sendMessage(s"${Prefix.SUCCESS}アセットを生成しました。")
    }
  } yield ()

  def findContainer(p: Player): SyncIO[Option[Container]] = SyncIO {
    Option(p.getTargetBlockExact(5))
      .flatMap(_.getState.downcastOrNone[Container])
      .headOption
  }

  def getConnectedContainer(world: World, data: ContainerData): SyncIO[Option[Container]] = {
    val program = for {
      facing    <- OptionT.fromOption[SyncIO](data.facing)
      chestType <- OptionT.fromOption[SyncIO](data.chestType)
      rotation  <- OptionT.fromOption[SyncIO](chestType match {
        case ChestData.Type.SINGLE => None
        case ChestData.Type.LEFT   => Some(+90)
        case ChestData.Type.RIGHT  => Some(-90)
      })

      location                   = data.location
      vector                     = Vector.of(facing.getDirection).rotate(rotation)
      connectedContainerLocation = (location + vector).toBukkit(world)

      container <- OptionT(SyncIO {
        world.getBlockAt(connectedContainerLocation).getState.downcastOrNone[Container & Chest]
      })

      ccFacing <- OptionT(SyncIO {
        val data = container.getBlockData()
        data.downcastOrNone[Directional].map(_.getFacing)
      })
      if ccFacing == facing

      ccChestType <- OptionT(SyncIO {
        val data = container.getBlockData()
        data.downcastOrNone[ChestData].map(_.getType)
      })
      if chestType match {
        case ChestData.Type.SINGLE => true
        case ChestData.Type.LEFT   => ccChestType == ChestData.Type.RIGHT
        case ChestData.Type.RIGHT  => ccChestType == ChestData.Type.LEFT
      }
    } yield container
    program.value
  }

  def getContainerData(container: Container): SyncIO[ContainerData] = SyncIO {
    val data = container.getBlockData()
    %(
      "location"    -> BlockLocation.of(container.getLocation()),
      "blockId"     -> container.getType().getKey().toString(),
      "name"        -> Option(container.getCustomName()),
      "facing"      -> data.downcastOrNone[Directional].map(_.getFacing),
      "waterlogged" -> data.downcastOrNone[Waterlogged].map(_.isWaterlogged),
      "chestType"   -> data.downcastOrNone[ChestData].map(_.getType),
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

  def convertToLootAsset[F[_]: Async](data: List[ContainerData], items: List[LootAssetItem]): F[LootAsset] = {
    val containers = data.map(_(unselect.name.inventory).to[LootAssetContainer])
    val uuid       = UUIDGen.randomUUID[F]
    uuid.map(LootAsset(None, _, data.head.name, containers, items))
  }
}
