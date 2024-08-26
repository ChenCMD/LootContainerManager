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
import cats.implicits.*

import dev.jorel.commandapi.CommandAPICommand
import dev.jorel.commandapi.CommandPermission
import dev.jorel.commandapi.executors.CommandArguments
import dev.jorel.commandapi.executors.PlayerCommandExecutor
import org.bukkit.entity.Player
import org.typelevel.log4cats.Logger

object CommandExecutor {
  def register[F[_]: Async](
    openedInventories: InventoriesStore[F],
    unsafeRunAsync: (errorHandler: Throwable => F[Unit]) => [U1] => (fa: F[U1]) => Unit
  )(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ) = {
    def handler(sender: Player)(e: Throwable): F[Unit] = {
      val (toSender, toLogger) = e match {
        case err: UserException          => (err.getMessage, None)
        case err: ConfigurationException => ("An error occurred while loading the configuration file.", Some(err))
        case err                         => ("An error occurred while executing the command.", Some(err))
      }
      for {
        _ <- Async[F].delay(sender.sendMessage(toSender))
        _ <- toLogger.traverse_(e => logger.error(e)(e.getMessage))
      } yield ()
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
