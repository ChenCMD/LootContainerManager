package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.GenLootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
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
  LAP: LootAssetPersistenceInstr[F]
) {
  def run(sender: CommandSender, args: List[String]): F[Unit] = {
    val p: Option[Player] = sender.downcastOrNone[Player]

    if (args.length == 0) {
      return Async[F]
        .whenA(sender.isOp)(UserException.raise(s"${Prefix.ERROR}/lcu <gen_asset>"))
        .as(sender.isOp)
    }

    args(0) match {
      case "gen_asset" =>
        val action = for {
          p <- OptionT.fromOption[F](p)
          _ <- OptionT.liftF(GenLootAsset.generateLootAsset(p))
        } yield ()
        action.value.void
      case _           => Async[F].unit
    }
  }
}
