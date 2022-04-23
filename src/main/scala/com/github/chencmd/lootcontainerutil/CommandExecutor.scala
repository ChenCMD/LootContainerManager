package com.github.chencmd.lootcontainerutil

import cats.data.OptionT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.github.chencmd.lootcontainerutil.Prefix
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

class CommandExecutor(ignorePlayerSet: IgnorePlayerSet)(using mcThread: OnMinecraftThread[IO]) {
  def run(sender: CommandSender, args: Array[String]): IO[Boolean] = {
    val p: Option[Player] = sender.downcastOrNone[Player]

    if (args.length == 0) {
      return IO.whenA(sender.isOp)(IO(sender.sendMessage(s"${Prefix.ERROR}/lcu <ignore>"))).as(sender.isOp)
    }

    val optIO = args(0) match {
      case "ignore" =>
        for {
          p <- OptionT.fromOption[IO](p)
          _ <- OptionT.liftF(ignorePlayerSet.registerIgnorePlayer(p))
          _ <- OptionT.liftF(IO {
            p.sendMessage(s"${Prefix.SUCCESS}ルートコンテナーの保護を8秒間無効化しました。")
          })
        } yield true
      case "gen_asset" =>
        for {
          p <- OptionT.fromOption[IO](p)
          _ <- OptionT.liftF(GenLootAsset.generateLootAsset(p))
        } yield true
      case _ => OptionT.pure[IO](false)
    }

    optIO.value.map(_.getOrElse(true))
  }
}
