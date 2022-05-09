package com.github.chencmd.lootcontainerutil

import cats.effect.{IO, SyncIO}
import cats.data.OptionT
import minecraft.OnMinecraftThread
import generic.extensions.CastOps.downcastOrNone
import org.bukkit.Location
import org.bukkit.block.{Block, Container}
import org.bukkit.entity.Player
import org.bukkit.loot.Lootable

import scala.jdk.CollectionConverters.*
import de.tr7zw.nbtapi.NBTTileEntity

object GenLootAsset {
  def generateLootAsset(p: Player)(using mcThread: OnMinecraftThread[IO]): IO[Unit] = IO {
    val acc = 5

    for {
      block <- OptionT(mcThread.run(SyncIO {
        val world = p.getWorld
        val loc = p.getLocation.clone()
        val vec = loc.getDirection.normalize().multiply(1d / acc)

        val res = ((3 * acc) until (10 * acc)).foldLeft[(Option[Container], Location)]((None, loc)) {
          case ((block, loc), _) => (
            block orElse world.getBlockAt(loc).getState.downcastOrNone[Container],
            loc.add(vec)
          )
        }
        res._1
      }))
      _ <- IO {
          for (item <- NBTTileEntity(block).getCompoundList("Items").asScala.toList) {
            val id = item.getString("id")
            val slot = item.getByte("Slot")

            /*
             * tag.TSB.ID -> {id:$tag.TSB.ID}
             * tag.TSB{Currency:1b} -> {PresetItem:"currency/"}
             * tag.TSB{Currency:2b} -> {PresetItem:"currency/high"}
             * tag.TSB{ShardRarity:1b} -> {PresetItem:"sacred_shard/lv-1"}
             * tag.TSB{ShardRarity:2b} -> {PresetItem:"sacred_shard/lv-2"}
             * tag.TSB{ShardRarity:3b} -> {PresetItem:"sacred_shard/lv-3"}
             * tag.TSB{ShardRarity:4b} -> {PresetItem:"sacred_shard/lv-4"}
             */
            val res = for {
              tag <- Option(item.getCompound("tag"))
              tsb <- tag.getCompound("tsb")


              res = ""
            } yield res

            res.getOrElse(item.toString)
          }
      }
    } yield ()
  }
}
