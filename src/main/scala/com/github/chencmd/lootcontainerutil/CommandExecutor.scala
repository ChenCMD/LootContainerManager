package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.GenLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

import cats.data.OptionT
import cats.effect.kernel.Async
import cats.implicits.*

import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

class CommandExecutor[F[_]: Async](using
  mcThread: OnMinecraftThread[F],
  Converter: ItemConversionInstr[F],
  LAPCI: LootAssetPersistenceCacheInstr[F]
) {
  def run(sender: CommandSender, args: List[String]): F[Unit] = {
    val p: Option[Player] = sender.downcastOrNone[Player]

    if (args.length == 0) {
      return Async[F]
        .whenA(sender.isOp)(UserException.raise(s"${Prefix.ERROR}/lcu <gen_asset>"))
        .as(sender.isOp)
    }

    val action = args(0) match {
      case "gen_asset" => for {
          p <- OptionT.fromOption[F](p)
          _ <- OptionT.liftF(GenLootAsset.generateLootAsset(p))
        } yield ()
      case _           => OptionT.none[F, Unit]
    }
    action.value.void
  }
}

object CommandExecutor {
  def apply[F[_]: Async](using
    mcThread: OnMinecraftThread[F],
    Converter: ItemConversionInstr[F],
    LAPCI: LootAssetPersistenceCacheInstr[F]
  ): F[CommandExecutor[F]] = Async[F].delay {
    new CommandExecutor[F]
  }
}
