package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import cats.effect.kernel.Sync
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil

type NMSIBlockData

object NMSIBlockData {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("world.level.block.state.IBlockData")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _getBlockMethod       = ReflectionUtil.getMethod(_clazz.getSuperclass, "b")
  def getBlockMethod[F[_]: Sync] = Sync[F].delay(_getBlockMethod)

  extension (nmsIBlockData: NMSIBlockData) {
    def getBlock[F[_]: Sync]: F[NMSBlock] = getBlockMethod.map { m =>
      m.invoke(nmsIBlockData).asInstanceOf[NMSBlock]
    }
  }
}
