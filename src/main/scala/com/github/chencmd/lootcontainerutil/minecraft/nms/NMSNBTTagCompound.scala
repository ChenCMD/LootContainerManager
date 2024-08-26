package com.github.chencmd.lootcontainermanager.minecraft.nms

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import java.lang.reflect.Constructor

type NMSNBTTagCompound <: NMSNBTTag

object NMSNBTTagCompound {
  lazy val _clazz       = ReflectionUtil.getMinecraftClass("nbt.NBTTagCompound")
  def clazz[F[_]: Sync] = Sync[F].delay(_clazz)

  lazy val _constructor       = _clazz
    .getDeclaredConstructor(classOf[java.util.Map[?, ?]])
    .tap(_.setAccessible(true))
    .asInstanceOf[Constructor[NMSNBTTagCompound]]
  def constructor[F[_]: Sync] = Sync[F].delay(_constructor)

  def apply[F[_]: Sync](map: Map[String, NMSNBTTag]): F[NMSNBTTagCompound] = for {
    constructor <- constructor
    compound    <- Sync[F].delay(constructor.newInstance(map.asJava))
  } yield compound
}
