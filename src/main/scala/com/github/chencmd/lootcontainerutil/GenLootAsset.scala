package com.github.chencmd.lootcontainerutil

import cats.effect.{IO, SyncIO}
import cats.data.OptionT
import minecraft.OnMinecraftThread
import generic.extensions.CastOps.downcastOrNone
import org.bukkit.{Bukkit, Location}
import org.bukkit.block.{Block, Container}
import org.bukkit.entity.Player
import org.bukkit.loot.Lootable

import scala.jdk.CollectionConverters.*
import de.tr7zw.nbtapi.NBTTileEntity

object GenLootAsset {
  def generateLootAsset(p: Player)(using mcThread: OnMinecraftThread[IO]): IO[Unit] = {
    val acc = 5

    val action = mcThread.run(SyncIO {
      val loc = p.getEyeLocation
      val vec = loc.getDirection.normalize().multiply(1d / acc)

      (0 until (5 * acc)).view
        .map(i => loc.clone().add(vec.clone().multiply(i)))
        .map(p.getWorld.getBlockAt)
        .flatMap(_.getState.downcastOrNone[Container])
        .headOption
        .foreach { block =>
          NBTTileEntity(block)
            .getCompoundList("Items")
            .asScala
            .toList
            .map(item => {
              val id = item.getString("id")
              val slot = item.getByte("Slot")
              val count = item.getByte("Count")
              val tag = Option(item.getCompound("tag"))
              p.sendMessage(id)

              /*
               * "tag.TSB.ID"           -> "{id:%%tag.TSB.ID%%}"
               * "tag.TSB{Currency:1b}" -> "{Count:%%Count%%,PresetItem:\"currency/\"}"
               * "tag.TSB{Currency:2b}" -> "{Count:%%Count%%,PresetItem:\"currency/high\"}"
               * "tag.TSB.ShardRarity"  -> "{Count:%%Count%%,PresetItem:\"sacred_shard/lv-%%tag.TSB.ShardRarity%%\"}"
               * "{}"                   -> "{Count:%%Count%%,id:%%id%%}"
               */
              // TODO Configデータを読み込み、NBTPathをパースしてそれを元にデータを構築する

            })
        }
    })

    action.void
  }
}
