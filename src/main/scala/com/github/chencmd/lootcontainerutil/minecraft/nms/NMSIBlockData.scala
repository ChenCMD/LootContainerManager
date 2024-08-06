package com.github.chencmd.lootcontainerutil.minecraft.nms

import cats.effect.kernel.Sync

import dev.array21.bukkitreflectionlib.ReflectionUtil

type NMSIBlockData

object NMSIBlockData {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("world.level.block.state.IBlockData")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)
}
