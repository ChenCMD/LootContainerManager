package com.github.chencmd.lootcontainerutil.feature.asset

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.Position
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.Vector

import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import org.bukkit.block.Container
import org.bukkit.block.data.Directional
import org.bukkit.block.data.Waterlogged
import org.bukkit.block.data.`type`.Chest
import org.bukkit.entity.Player

object GenLootAsset {
  val acc = 5
  def generateLootAsset[F[_]: Async](
    p: Player
  )(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAP: LootAssetPersistenceInstr[F]
  ): F[Unit] = {
    val action = for {
      blockDataOpt                                                     <- mcThread.run(SyncIO {
        val loc           = p.getEyeLocation
        val vec           = Vector.of(loc.getDirection).normalize * (1d / acc)
        val w             = p.getWorld
        val targetedBlock = (0 until (5 * acc)).view
          .map(i => w.getBlockAt((Position.of(loc) + vec * i).toBukkit))
          .flatMap(_.getState.downcastOrNone[Container])
          .headOption

        targetedBlock.map { block =>
          val data = block.getBlockData()
          (
            BlockLocation.of(block.getLocation()),
            block.getType().getKey().toString(),
            Option(block.getCustomName()),
            data.downcastOrNone[Directional].map(_.getFacing),
            data.downcastOrNone[Waterlogged].map(_.isWaterlogged),
            data.downcastOrNone[Chest].map(_.getType),
            block.getInventory().getContents().toList
          )
        }
      })
      (location, blockId, name, facing, waterlogged, chestType, items) <-
        blockDataOpt.fold(UserException.raise("No container was found."))(_.pure[F])

      asset <- items
        .traverseWithIndexM { (item, slot) =>
          Converter.toItemIdentifier(item).map(LootAssetItem(slot, _, item.getAmount()))
        }
        .map(LootAsset(location, blockId, name, facing, waterlogged, chestType, _))

      _ <- LAP.storeLootAsset(asset)
    } yield ()

    action
  }
}
