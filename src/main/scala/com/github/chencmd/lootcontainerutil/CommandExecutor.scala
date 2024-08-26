package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.DelLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.GenLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.terms.InventoriesStore

import cats.effect.kernel.Async

import dev.jorel.commandapi.CommandAPICommand
import dev.jorel.commandapi.CommandPermission
import dev.jorel.commandapi.executors.CommandArguments
import dev.jorel.commandapi.executors.PlayerCommandExecutor
import java.util.logging.Level
import org.bukkit.Bukkit
import org.bukkit.entity.Player

object CommandExecutor {
  def register[F[_]: Async](
    openedInventories: InventoriesStore[F],
    unsafeRunAsync: [U] => (errorHandler: Throwable => U) => [U1] => (fa: F[U1]) => Unit
  )(using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ) = {
    def handler(sender: Player)(e: Throwable)                    = e match {
      case err: UserException          => sender.sendMessage(err.getMessage)
      case err: ConfigurationException =>
        sender.sendMessage("An error occurred while loading the configuration file.")
        Bukkit.getLogger.log(Level.SEVERE, err.getMessage, err)
      case err                         =>
        sender.sendMessage("An error occurred while executing the command.")
        Bukkit.getLogger.log(Level.SEVERE, err.getMessage, err)
    }
    def genExecutor(f: Player => F[Unit]): PlayerCommandExecutor = { (sender: Player, _: CommandArguments) =>
      unsafeRunAsync(handler(sender))(f(sender))
    }

    val genAsset = CommandAPICommand("gen_asset")
      .withAliases("g")
      .executesPlayer(genExecutor(GenLootAsset.generateLootAsset))
    val delAsset = CommandAPICommand("del_asset")
      .withAliases("d")
      .executesPlayer(genExecutor(DelLootAsset.deleteLootAsset(_, openedInventories)))

    Async[F].delay {
      CommandAPICommand("lootcontainerutil")
        .withAliases("lcu")
        .withPermission(CommandPermission.OP)
        .withSubcommands(genAsset, delAsset)
        .register()
    }
  }
}
