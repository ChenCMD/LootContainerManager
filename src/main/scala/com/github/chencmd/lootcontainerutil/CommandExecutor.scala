package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.DelLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.GenLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession

import cats.effect.kernel.Async
import cats.effect.kernel.Ref

import org.bukkit.entity.Player
import dev.jorel.commandapi.CommandAPICommand
import dev.jorel.commandapi.CommandPermission
import dev.jorel.commandapi.executors.CommandArguments
import org.bukkit.Bukkit
import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import java.util.logging.Level

object CommandExecutor {
  def register[F[_]: Async](
    openedInventories: Ref[F, Map[BlockLocation, InventorySession]],
    unsafeRunAsync: [U] => (errorHandler: Throwable => U) => [U1] => (fa: F[U1]) => Unit
  )(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ) = {
    def handler(sender: Player)(e: Throwable) = e match {
      case err: UserException          => sender.sendMessage(err.getMessage)
      case err: ConfigurationException =>
        sender.sendMessage("An error occurred while loading the configuration file.")
        Bukkit.getLogger.log(Level.SEVERE, err.getMessage, err)
      case err                         =>
        sender.sendMessage("An error occurred while executing the command.")
        Bukkit.getLogger.log(Level.SEVERE, err.getMessage, err)
    }

    val genAsset = CommandAPICommand("gen_asset").executesPlayer { (player: Player, _: CommandArguments) =>
      unsafeRunAsync(handler(player))(GenLootAsset.generateLootAsset(player))
    }
    val delAsset = CommandAPICommand("del_asset").executesPlayer { (player: Player, _: CommandArguments) =>
      unsafeRunAsync(handler(player))(DelLootAsset.deleteLootAsset(player, openedInventories))
    }

    Async[F].delay {
      CommandAPICommand("lcu")
        .withPermission(CommandPermission.OP)
        .withSubcommands(genAsset, delAsset)
        .register()
    }
  }
}
