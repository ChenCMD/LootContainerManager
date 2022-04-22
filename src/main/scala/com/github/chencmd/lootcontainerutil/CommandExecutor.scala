package com.github.chencmd.lootcontainerutil

import cats.data.OptionT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

class CommandExecutor(ignorePlayerSet: IgnorePlayerSet) {
  def run(sender: CommandSender, args: Array[String]): Boolean = {
    val p: Option[Player] = sender match {
      case p: Player => Some(p)
      case _ => None
    }

    if (args.length == 0) {
      if (sender.isOp) {
        sender.sendMessage(s"${Prefix.ERROR}/lcu <ignore>")
      } else {
        return false
      }
    }

    args(0) match {
      case "ignore" =>
        val action = for {
          p <- OptionT.fromOption[IO](p)
          _ <- OptionT.liftF(ignorePlayerSet.registerIgnorePlayer(p))
          _ <- OptionT.liftF(IO {
            p.sendMessage(s"${Prefix.SUCCESS}ルートコンテナーの保護を8秒間無効化しました。")
          })
        } yield ()

        action.value.unsafeRunAndForget()
        true
    }
  }
}
