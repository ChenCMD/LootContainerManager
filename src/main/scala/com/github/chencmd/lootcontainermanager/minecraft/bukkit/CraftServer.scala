package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.minecraft.nms.NMSMinecraftServer

import cats.effect.kernel.Sync
import cats.implicits.*

import org.bukkit.Server

import dev.array21.bukkitreflectionlib.ReflectionUtil

type CraftServer >: Server
object CraftServer {
  private lazy val _clazz = ReflectionUtil.getBukkitClass("CraftServer").asInstanceOf[Class[CraftServer]]
  def clazz[F[_]: Sync]   = Sync[F].delay(_clazz)

  lazy val _getServerMethod       = ReflectionUtil.getMethod(_clazz, "getServer")
  def getServerMethod[F[_]: Sync] = Sync[F].delay(_getServerMethod)

  def cast[F[_]: Sync](server: Server): F[CraftServer] = clazz.map(_.cast(server))

  extension (server: CraftServer) {
    def getServer[F[_]: Sync]: F[NMSMinecraftServer] = getServerMethod.map { m =>
      m.invoke(server).asInstanceOf[NMSMinecraftServer]
    }
  }
}
