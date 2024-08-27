package com.github.chencmd.lootcontainermanager

import com.github.chencmd.lootcontainermanager.exceptions.ConfigurationException
import com.github.chencmd.lootcontainermanager.exceptions.UserException
import com.github.chencmd.lootcontainermanager.feature.asset.DelLootAsset
import com.github.chencmd.lootcontainermanager.feature.asset.GenLootAsset
import com.github.chencmd.lootcontainermanager.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.terms.InventoriesStore

import cats.effect.kernel.Async
import cats.implicits.*
import org.typelevel.log4cats.Logger

import org.bukkit.entity.Player

import dev.jorel.commandapi.CommandAPICommand
import dev.jorel.commandapi.CommandPermission
import dev.jorel.commandapi.executors.CommandArguments
import dev.jorel.commandapi.executors.PlayerCommandExecutor

object CommandExecutor {
  def register[F[_]: Async](
    openedInventories: InventoriesStore[F],
    unsafeRunAsync: [U1] => (fa: F[U1]) => Unit
  )(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ) = {
    def handler(sender: Player): PartialFunction[Throwable, F[Unit]] = { (e: Throwable) =>
      val (toSender, toLogger) = e match {
        case err: UserException          => (err.getMessage, None)
        case err: ConfigurationException => (s"${Prefix.ERROR}設定ファイルの読み込み中にエラーが発生しました。", Some(err))
        case err                         => (s"${Prefix.ERROR}コマンドの実行中にエラーが発生しました。", Some(err))
      }
      for {
        _ <- Async[F].delay(sender.sendMessage(toSender))
        _ <- toLogger.traverse_(e => logger.error(e)(e.getMessage))
      } yield ()
    }

    def genExecutor(f: Player => F[Unit]): PlayerCommandExecutor = { (sender: Player, _: CommandArguments) =>
      unsafeRunAsync(f(sender).handleErrorWith(handler(sender)))
    }

    val genAsset = CommandAPICommand("gen_asset")
      .withAliases("g")
      .executesPlayer(genExecutor(GenLootAsset.generateLootAsset))
    val delAsset = CommandAPICommand("del_asset")
      .withAliases("d")
      .executesPlayer(genExecutor(DelLootAsset.deleteLootAsset(_, openedInventories)))

    Async[F].delay {
      CommandAPICommand("lootcontainermanager")
        .withAliases("lcm")
        .withPermission(CommandPermission.OP)
        .withSubcommands(genAsset, delAsset)
        .register()
    }
  }
}
