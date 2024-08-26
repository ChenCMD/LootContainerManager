package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.minecraft.nms.NMSIBlockData

import cats.effect.kernel.Sync
import cats.implicits.*

import org.bukkit.block.Block

import dev.array21.bukkitreflectionlib.ReflectionUtil

type CraftBlock >: Block
object CraftBlock {
  private lazy val _clazz = ReflectionUtil.getBukkitClass("block.CraftBlock").asInstanceOf[Class[CraftBlock]]
  def clazz[F[_]: Sync]   = Sync[F].delay(_clazz)

  lazy val _getNMSMethod       = ReflectionUtil.getMethod(_clazz, "getNMS")
  def getNMSMethod[F[_]: Sync] = Sync[F].delay(_getNMSMethod)

  def cast[F[_]: Sync](block: Block): F[CraftBlock] = clazz.map(_.cast(block))

  extension (block: CraftBlock) {
    def getNMS[F[_]: Sync]: F[NMSIBlockData] = getNMSMethod.map { m =>
      m.invoke(block).asInstanceOf[NMSIBlockData]
    }
  }
}
