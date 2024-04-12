package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.SyncIO
import cats.implicits.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import java.lang.reflect.Constructor

type NMSItemStack

object NMSItemStack {
  private lazy val _clazz = ReflectionUtil.getMinecraftClass("net.minecraft.nbt.ItemStack")
  def clazz               = SyncIO(_clazz)

  private lazy val _constructor = ReflectionUtil
    .getConstructor(_clazz, NMSNBTTagCompound._clazz)
    .asInstanceOf[Constructor[NMSItemStack]]
  def constructor               = SyncIO(_constructor)

  def apply(nbt: NBTTag.NBTTagCompound): SyncIO[NMSItemStack] = for {
    tag         <- NMSNBTTag.convert(nbt)
    constructor <- constructor
    nmsItem     <- SyncIO(constructor.newInstance(tag))
  } yield nmsItem
}
