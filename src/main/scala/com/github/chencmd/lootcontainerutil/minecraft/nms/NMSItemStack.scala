package com.github.chencmd.lootcontainermanager.minecraft.nms

import com.github.chencmd.lootcontainermanager
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.util.chaining.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import java.lang.reflect.Constructor

type NMSItemStack

object NMSItemStack {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("world.item.ItemStack")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _constructor       = _clazz
    .getDeclaredConstructor(NMSNBTTagCompound._clazz)
    .tap(_.setAccessible(true))
    .asInstanceOf[Constructor[NMSItemStack]]
  def constructor[F[_]: Sync] = Sync[F].delay(_constructor)

  def apply[F[_]: Sync](nbt: NBTTag.NBTTagCompound): F[NMSItemStack] = for {
    tag         <- NMSNBTTag.convert(nbt)
    constructor <- constructor
    nmsItem     <- Sync[F].delay(constructor.newInstance(tag))
  } yield nmsItem
}
