package com.github.chencmd.lootcontainermanager.minecraft.bukkit
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.RemoteControlCommandListener

import cats.effect.kernel.Sync
import cats.implicits.*

import java.net.SocketAddress

import org.bukkit.Bukkit
import org.bukkit.command.CommandSender

object VoidCommandSender {
  def createVoidSender[G[_]: Sync]: G[CommandSender] = for {
    craftServer <- CraftServer.cast(Bukkit.getServer)
    mcServer    <- craftServer.getServer
    socketAddress = new SocketAddress {}
    rconCommandListener <- RemoteControlCommandListener(mcServer, socketAddress)
    remoteConsole       <- rconCommandListener.getRemoteConsole
  } yield remoteConsole
}
