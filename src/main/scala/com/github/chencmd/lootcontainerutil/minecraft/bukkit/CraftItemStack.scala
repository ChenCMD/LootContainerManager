package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil

import cats.effect.kernel.Sync
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import org.bukkit.inventory.ItemStack

type CraftItemStack <: ItemStack

object CraftItemStack {
  lazy val _clazz       = ReflectionUtil.getBukkitClass("inventory.CraftItemStack")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _asCraftMirrorMethod       = ReflectionUtil.getMethod(_clazz, "asCraftMirror", NMSItemStack._clazz)
  def asCraftMirrorMethod[F[_]: Sync] = Sync[F].delay(_asCraftMirrorMethod)

  def asCraftMirror[F[_]: Sync](item: NMSItemStack): F[CraftItemStack] = asCraftMirrorMethod.map { m =>
    m.invoke(null, item).asInstanceOf[CraftItemStack]

  }
}
