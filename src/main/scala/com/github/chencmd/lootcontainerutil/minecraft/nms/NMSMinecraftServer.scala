package com.github.chencmd.lootcontainerutil.minecraft.nms

import cats.effect.kernel.Sync

import dev.array21.bukkitreflectionlib.ReflectionUtil

type NMSMinecraftServer
object NMSMinecraftServer {
  lazy val _clazz = ReflectionUtil.getMinecraftClass("server.MinecraftServer")
  def clazz[F[_]: Sync]   = Sync[F].delay(_clazz)
}
