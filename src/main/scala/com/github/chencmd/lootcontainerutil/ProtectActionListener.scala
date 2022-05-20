package com.github.chencmd.lootcontainerutil

import cats.effect.{IO, SyncIO}
import cats.effect.unsafe.implicits.global
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import org.bukkit.{Bukkit, ChatColor, GameRule}
import org.bukkit.block.Container
import org.bukkit.entity.Player
import org.bukkit.event.inventory.InventoryOpenEvent
import org.bukkit.event.world.LootGenerateEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.loot.{LootTable, Lootable}
import org.bukkit.metadata.{FixedMetadataValue, MetadataValue}
import org.bukkit.plugin.java.JavaPlugin
import scala.concurrent.duration.*

import scala.util.chaining.*
import scala.jdk.CollectionConverters.*
import org.bukkit.Sound
import org.bukkit.SoundCategory

class ProtectActionListener(plugin: JavaPlugin, ignorePlayerSet: IgnorePlayerSet)(using
    mcThread: OnMinecraftThread[IO]
) extends Listener {
  Bukkit.getPluginManager.registerEvents(this, plugin)

  @EventHandler def onLootGenerate(e: LootGenerateEvent): Unit = {
    val pOpt = e.getEntity.downcastOrNone[Player]

    if (pOpt.isEmpty) {
      return
    }

    val p: Player = pOpt.get

    val action = for {
      isIgnoreProtect <- ignorePlayerSet.isIgnorePlayer(p)

      _ <- IO.unlessA(isIgnoreProtect) {
        val lootTable = e.getLootTable

        for {
          _ <- mcThread.runAndForget(SyncIO {
            Option(e.getInventoryHolder)
              .flatMap(_.downcastOrNone[Container])
              .flatMap(_.downcastOrNone[Lootable])
              .tapEach(_.setLootTable(lootTable))
              .tapEach(_.update())
          })
          _ <- {
            val loc = p.getLocation
            for {
              _ <- IO(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
              _ <- IO.sleep((2 * 0.05).seconds)
              _ <- IO(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
            } yield ()
          }.start
          _ <- IO {
            p.sendMessage(s"${Prefix.INFO}ルートテーブルが設定されているため開くことができませんでした。")
            p.sendMessage(
              s"${Prefix.INFO}意図して開く場合は、${ChatColor.GOLD}/lcu ignore${ChatColor.WHITE}を実行してください。"
            )
            p.sendMessage(s"${Prefix.INFO}設定されているルートテーブル: ${lootTable.getKey}")
            p.setMetadata("generateCancelled", FixedMetadataValue(plugin, e.getWorld.getGameTime))
          }
        } yield ()
      }
    } yield isIgnoreProtect

    val isOpenable = action.unsafeRunSync()
    if (!isOpenable) {
      e.setCancelled(true)
    }
  }

  @EventHandler def onContainerOpen(e: InventoryOpenEvent): Unit = {

    val p = e.getPlayer.asInstanceOf[Player]

    val action = IO {
      val res = p
        .getMetadata("generateCancelled")
        .asScala
        .headOption
        .flatMap(_.value.downcastOrNone[Long])
        .exists(_ == e.getPlayer.getWorld.getGameTime)
      p.removeMetadata("generateCancelled", plugin)
      res
    }

    val isCancelled = action.unsafeRunSync()
    if (isCancelled) {
      e.setCancelled(true)
    }
  }
}
