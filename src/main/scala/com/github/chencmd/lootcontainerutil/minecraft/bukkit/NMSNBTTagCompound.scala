package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import cats.effect.SyncIO
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import java.lang.reflect.Constructor

type NMSNBTTagCompound <: NMSNBTTag

object NMSNBTTagCompound {
  lazy val _clazz = ReflectionUtil.getMinecraftClass("nbt.CompoundTag")
  def clazz       = SyncIO(_clazz)

  lazy val _constructor = ReflectionUtil
    .getConstructor(_clazz, classOf[Map[?, ?]])
    .asInstanceOf[Constructor[NMSNBTTagCompound]]
  def constructor       = SyncIO(_constructor)

  def apply(map: Map[String, NMSNBTTag]): SyncIO[NMSNBTTagCompound] = for {
    constructor <- constructor
    compound    <- SyncIO(constructor.newInstance(map.asJava))
  } yield compound
}
