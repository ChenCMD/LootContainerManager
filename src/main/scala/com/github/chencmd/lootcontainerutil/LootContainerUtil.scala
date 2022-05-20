package com.github.chencmd.lootcontainerutil

import cats.effect.IO
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import org.bukkit.Bukkit
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.plugin.java.JavaPlugin

class LootContainerUtil extends JavaPlugin {
  var cmdExecutor: Option[CommandExecutor] = None

  override def onEnable(): Unit = {
    Bukkit.getConsoleSender.sendMessage("LootContainerUtil enabled.")

    given mcThread: OnMinecraftThread[IO] = new OnMinecraftThread[IO](this)

    val ignorePlayerSet = new IgnorePlayerSet()

    new ProtectActionListener(this, ignorePlayerSet)
    cmdExecutor = Some(new CommandExecutor(ignorePlayerSet))
  }

  override def onCommand(
      sender: CommandSender,
      command: Command,
      label: String,
      args: Array[String]
  ): Boolean = {
    if (command.getName == "lcu") {
      cmdExecutor.get.run(sender, args)
    } else {
      false
    }
  }
}
