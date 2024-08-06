package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil
import com.github.chencmd.lootcontainerutil.minecraft.nms.NMSItemStack

import cats.effect.kernel.Sync
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import org.bukkit.inventory.ItemStack

object CraftItemStack {
  lazy val _clazz       = ReflectionUtil.getBukkitClass("inventory.CraftItemStack")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _asBukkitCopyMethod       = ReflectionUtil.getMethod(_clazz, "asBukkitCopy", NMSItemStack._clazz)
  def asBukkitCopyMethod[F[_]: Sync] = Sync[F].delay(_asBukkitCopyMethod)

  def asBukkitCopy[F[_]: Sync](item: NMSItemStack): F[ItemStack] = asBukkitCopyMethod.map { m =>
    m.invoke(null, item).asInstanceOf[ItemStack]
  }
}
