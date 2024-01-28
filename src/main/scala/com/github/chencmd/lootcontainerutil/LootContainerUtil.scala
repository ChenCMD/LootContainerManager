package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

import cats.implicits.*

import org.bukkit.Bukkit
import org.bukkit.command.Command
import org.bukkit.command.CommandSender
import cats.effect.kernel.Ref
import cats.effect.kernel.Async

class LootContainerUtil extends IOJavaPlugin {
  val cmdExecutor: Ref[F, Option[CommandExecutor]] = Ref.unsafe(None)

  override def onEnableIO() = Effect.Sync {
    given OnMinecraftThread[F] = new OnMinecraftThread[F](this)
    val ignorePlayerSet        = new IgnorePlayerSet()
    for {
      _   <- Async[F].delay {
        Bukkit.getPluginManager.registerEvents(new ProtectActionListener(this, ignorePlayerSet), this)
      }
      res <- cmdExecutor.set(Some(new CommandExecutor(ignorePlayerSet)))
      _   <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("LootContainerUtil enabled."))
    } yield res
  }

  override def onCommandIO(
    sender: CommandSender,
    command: Command,
    label: String,
    args: Array[String]
  ) = Effect.Sync {
    if (command.getName == "lcu") {
      cmdExecutor.get.map(_.map(_.run(sender, args)).getOrElse(false))
    } else {
      false.pure[F]
    }
  }
}
