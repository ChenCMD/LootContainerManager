package com.github.chencmd.lootcontainerutil.feature.genasset

import com.github.chencmd.lootcontainerutil.Config
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.Location
import com.github.chencmd.lootcontainerutil.minecraft.Vector
import com.github.chencmd.lootcontainerutil.nbt.NBTTagParser
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*
import cats.mtl.Raise

import scala.jdk.CollectionConverters.*

import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest
import org.bukkit.block.data.Directional
import org.bukkit.block.data.Waterlogged
import de.tr7zw.nbtapi.NBTTileEntity
import org.bukkit.block.Container
import org.bukkit.entity.Player

object GenLootAsset {
  val acc = 5
  def generateLootAsset[F[_]: Async](
    p: Player
  )(using R: Raise[F, String], mcThread: OnMinecraftThread[F], config: Config): F[Unit] = {
    val action = for {
      blockDataOpt                                               <- mcThread.run(SyncIO {
        val loc           = p.getEyeLocation
        val vec           = Vector(loc.getDirection).normalize * (1d / acc)
        val w             = p.getWorld
        val targetedBlock = (0 until (5 * acc)).view
          .map(i => w.getBlockAt((Location(loc) + vec * i).toBukkit))
          .flatMap(_.getState.downcastOrNone[Container])
          .headOption

        targetedBlock.map { block =>
          val data = block.getBlockData()
          (
            Location(block.getLocation()),
            block.getType().getKey().toString(),
            data.downcastOrNone[Directional].map(_.getFacing),
            data.downcastOrNone[Waterlogged].map(_.isWaterlogged),
            data.downcastOrNone[Chest].map(_.getType),
            NBTTileEntity(block).getCompoundList("Items").asScala.toList
          )
        }
      })
      (location, blockId, facing, waterlogged, chestType, items) <-
        blockDataOpt.fold(R.raise("No container was found."))(_.pure[F])

      // _ <- Async[F].delay {
      //   p.sendMessage(s"location: $location")
      //   p.sendMessage(s"blockId: $blockId")
      //   p.sendMessage(s"facing: $facing")
      //   p.sendMessage(s"waterlogged: $waterlogged")
      //   p.sendMessage(s"chestType: $chestType")
      //   p.sendMessage(s"items: $items")
      // }

      /*
       * "tag.TSB.ID"           -> ["artifact:%%tag.TSB.ID%%",                       "1",         ""       ]
       * "tag.TSB{Currency:1b}" -> ["preset:currency/",                              "%%Count%%", ""       ]
       * "tag.TSB{Currency:2b}" -> ["preset:currency/high",                          "%%Count%%", ""       ]
       * "tag.TSB.ShardRarity"  -> ["preset:artifact_shard/%%tag.TSB.ShardRarity%%", "%%Count%%", ""       ]
       * "{}"                   -> ["%%id%%",                                        "%%Count%%", "%%tag%%"]
       */

      stringifiedItems <- items
        .traverse(i => NBTTagParser.parse(i.toString).fold(R.raise, _.pure[F]))
        .flatMap(_.traverse {
          case NBTTag.NBTTagCompound(item) => for {
              tag <- item
                .get("tag")
                .traverse(_.downcastOrRaise[NBTTag.NBTTagCompound][F]())
                .map(_.getOrElse(NBTTag.NBTTagCompound(Map.empty)): NBTTag.NBTTagCompound)

              (_, usingInterpolations) <- config.itemMappers
                .find(_._1.isAccessible(tag))
                .fold(R.raise(s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

              interpolatedTag <- usingInterpolations
                .traverse(_.interpolate(NBTTag.NBTTagCompound(item.updated("tag", tag))))
                .fold(R.raise("itemMapper did not return a result."))(_.pure[F])
            } yield interpolatedTag
        })

      rowData = {
        location.w.getKey.toString
          :: blockId
          :: NBTTag.NBTTagDouble(location.x).toSNBT
          :: NBTTag.NBTTagDouble(location.y).toSNBT
          :: NBTTag.NBTTagDouble(location.z).toSNBT
          :: facing.map(_.toString().toLowerCase()).orEmpty
          :: waterlogged.map(_.toString()).orEmpty
          :: chestType.map(_.toString().toLowerCase()).orEmpty
          :: stringifiedItems.flatten
      }

      _ <- Async[F].delay(
        p.sendMessage(rowData.mkString(", "))
      )
    } yield ()

    action
  }
}
