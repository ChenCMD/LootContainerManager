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
import cats.effect.kernel.Sync
import cats.implicits.*
import org.typelevel.log4cats.Logger

import org.bukkit.entity.Player

import dev.jorel.commandapi.CommandAPICommand
import dev.jorel.commandapi.CommandPermission
import dev.jorel.commandapi.executors.CommandArguments
import dev.jorel.commandapi.executors.PlayerCommandExecutor
import cats.effect.kernel.Ref
import java.util.UUID
import org.bukkit.Sound
import org.bukkit.SoundCategory

object CommandExecutor {
  def register[F[_]: Async, G[_]: Sync](
    openedInventories: InventoriesStore[F],
    highlightDisablePlayers: Ref[F, Set[UUID]],
    unsafeRunAsync: [U1] => (fa: F[U1]) => Unit,
    debug: Boolean
  )(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F, G],
    Converter: ItemConversionInstr[F, G],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ): F[Unit] = {
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

    val genAsset  = CommandAPICommand("gen_asset")
      .withAliases("g")
      .executesPlayer(genExecutor(GenLootAsset.generateLootAsset[F, G]))
    val delAsset  = CommandAPICommand("del_asset")
      .withAliases("d")
      .executesPlayer(genExecutor(DelLootAsset.deleteLootAsset[F, G](_, openedInventories, debug)))
    val highlight = CommandAPICommand("highlight")
      .withAliases("h")
      .executesPlayer(genExecutor { p =>
        for {
          highlightEnabled <- highlightDisablePlayers.modify {
            case set if set.contains(p.getUniqueId) => (set - p.getUniqueId, true)
            case set                                => (set + p.getUniqueId, false)
          }
      _ <- Async[F].delay(p.playSound(p, Sound.UI_BUTTON_CLICK, SoundCategory.MASTER, 0.6f, 1.2f))
          _                <- Async[F].delay {
            if (highlightEnabled) {
              p.sendMessage(s"${Prefix.INFO}ハイライトを有効化しました。")
            } else {
              p.sendMessage(s"${Prefix.INFO}ハイライトを無効化しました。")
            }
          }
        } yield ()
      })

    Async[F].delay {
      CommandAPICommand("lootcontainermanager")
        .withAliases("lcm")
        .withPermission(CommandPermission.OP)
        .withSubcommands(genAsset, delAsset, highlight)
        .register()
    }
  }
}
