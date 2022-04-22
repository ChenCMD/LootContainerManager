package com.github.chencmd.lootcontainerutil

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone
import org.bukkit.{Bukkit, ChatColor}
import org.bukkit.block.Container
import org.bukkit.entity.Player
import org.bukkit.event.inventory.InventoryOpenEvent
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.loot.{LootTable, Lootable}
import org.bukkit.plugin.java.JavaPlugin

class ProtectActionListener(plugin: JavaPlugin, ignorePlayerSet: IgnorePlayerSet) extends Listener {
  Bukkit.getPluginManager.registerEvents(this, plugin)

  @EventHandler def onContainerOpen(e: InventoryOpenEvent): Unit = {
    val setLootTable: Option[LootTable] = Option(e.getInventory.getHolder)
      .flatMap(_.downcastOrNone[Container])
      .flatMap(_.downcastOrNone[Lootable])
      .flatMap(inv => Option(inv.getLootTable))

    if (setLootTable.isEmpty) {
      return
    }

    val p = e.getPlayer.asInstanceOf[Player]

    val action = for {
      isIgnoreProtect <- ignorePlayerSet.isIgnorePlayer(p)
      _ <- IO.whenA(!isIgnoreProtect)(IO {
        p.sendMessage(s"${Prefix.INFO}ルートテーブルが設定されているため開くことができませんでした。")
        p.sendMessage(s"${Prefix.INFO}意図して開く場合は、${ChatColor.GOLD}/lcu ignore${ChatColor.WHITE}を実行してください。")
        p.sendMessage(s"${Prefix.INFO}設定されているルートテーブル: ${setLootTable.get.getKey}")
      })
    } yield isIgnoreProtect

    val isIgnoreProtect = action.unsafeRunSync()
    if (!isIgnoreProtect) {
      e.setCancelled(true)
    }
  }
}
