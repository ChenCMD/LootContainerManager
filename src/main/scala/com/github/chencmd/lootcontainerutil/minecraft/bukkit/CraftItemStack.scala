package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil

import cats.effect.SyncIO
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import org.bukkit.inventory.ItemStack

type CraftItemStack <: ItemStack

object CraftItemStack {
  private lazy val clazz = ReflectionUtil.getBukkitClass("inventory.CraftItemStack")

  private lazy val asCraftMirrorMethod = ReflectionUtil.getMethod(clazz, "asCraftMirror")

  def asCraftMirror(item: NMSItemStack): SyncIO[CraftItemStack] = SyncIO {
    asCraftMirrorMethod.invoke(null, item).asInstanceOf[CraftItemStack]
  }
}
