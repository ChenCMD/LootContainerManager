package com.github.chencmd.lootcontainermanager.minecraft.nms

import com.github.chencmd.lootcontainermanager

import cats.effect.kernel.Sync
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil

type NMSBlock

object NMSBlock {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("world.level.block.Block")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _getIdMethod       = ReflectionUtil.getMethod(_clazz, "i", NMSIBlockData._clazz)
  def getIdMethod[F[_]: Sync] = Sync[F].delay(_getIdMethod)

  def getId[F[_]: Sync](block: NMSIBlockData): F[Int] = getIdMethod.map { m =>
    m.invoke(null, block).asInstanceOf[Int]
  }
}
