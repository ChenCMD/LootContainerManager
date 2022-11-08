package com.github.chencmd.lootcontainerutil

import cats.data.OptionT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import generic.extensions.CastOps.downcastOrNone
import minecraft.OnMinecraftThread
import de.tr7zw.nbtapi.NbtApiException
import org.bukkit.Bukkit
import concurrent.duration.*

class CommandExecutor(ignorePlayerSet: IgnorePlayerSet)(using mcThread: OnMinecraftThread[IO]) {
  def run(sender: CommandSender, args: Array[String]): Boolean = {
    val p: Option[Player] = sender.downcastOrNone[Player]

    if (args.length == 0) {
      return IO
        .whenA(sender.isOp)(IO(sender.sendMessage(s"${Prefix.ERROR}/lcu <ignore>")))
        .as(sender.isOp)
        .unsafeRunSync()
    }

    args(0) match {
      case "ignore" =>
        val action = for {
          p <- OptionT.fromOption[IO](p)
          _ <- OptionT.liftF(ignorePlayerSet.registerIgnorePlayer(p))
          _ <- OptionT.liftF {
            IO.sleep((8 * 20).second) *> ignorePlayerSet.removeIgnorePlayer(p).start
          }
          _ <- OptionT.liftF(IO {
            p.sendMessage(s"${Prefix.SUCCESS}ルートコンテナーの保護を8秒間無効化しました。")
          })
        } yield true
        action.value.map(_.getOrElse(false)).unsafeRunSync()
      case "gen_asset" =>
        val action = for {
          p <- OptionT.fromOption[IO](p)
          _ <- OptionT.liftF(GenLootAsset.generateLootAsset(p))
        } yield ()
        action.value.unsafeRunAsync(
          _.left.toOption
            .foreach(_.printStackTrace())
        )
        true
      case _ => false
    }
  }
}
