package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.minecraft.nms.NMSMinecraftServer

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.util.chaining.*

import java.lang.reflect.Constructor
import java.net.SocketAddress

import org.bukkit.command.CommandSender

import dev.array21.bukkitreflectionlib.ReflectionUtil

type RemoteControlCommandListener
object RemoteControlCommandListener {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("server.rcon.RemoteControlCommandListener")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _constructor       = _clazz
    .getDeclaredConstructor(NMSMinecraftServer._clazz, classOf[SocketAddress])
    .tap(_.setAccessible(true))
    .asInstanceOf[Constructor[RemoteControlCommandListener]]
  def constructor[F[_]: Sync] = Sync[F].delay(_constructor)

  lazy val _remoteConsoleField       = ReflectionUtil.getField(_clazz, "remoteConsole")
  def remoteConsoleField[F[_]: Sync] = Sync[F].delay(_remoteConsoleField)

  def apply[F[_]: Sync](s: NMSMinecraftServer, sa: SocketAddress): F[RemoteControlCommandListener] =
    constructor.map { c =>
      c.newInstance(s, sa)
    }

  extension (r: RemoteControlCommandListener) {
    def getRemoteConsole[F[_]: Sync]: F[CommandSender] = remoteConsoleField.map { f =>
      f.get(r).asInstanceOf[CommandSender]
    }
  }
}
