package com.github.chencmd.lootcontainermanager.nbt.definition

import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag.NBTTagCompound

import cats.implicits.*

case class NBTPathInterpolation(firstPart: String, pairRest: List[(NBTPath, String)]) {
  def interpolate(compound: NBTTagCompound): Option[String] = pairRest
    .traverse {
      case (path, str) => path
          .access(compound)
          .pure[Option]
          .filter(_.nonEmpty)
          .map(_.map(_.toRawString).mkString(",") + str)
    }
    .map(firstPart + _.mkString)
}
