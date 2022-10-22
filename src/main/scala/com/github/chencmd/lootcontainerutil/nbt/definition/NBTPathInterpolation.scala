package com.github.chencmd.lootcontainerutil.nbt.definition

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound
import cats.implicits.given

case class NBTPathInterpolation(
    firstPart: String,
    pairRest: List[(NBTPath, String)])
    derives CanEqual {
  def interpolate(compound: NBTTagCompound): Option[String] =
    pairRest
      .traverse { case (path: NBTPath, str) =>
        val data = path.access(compound)
        Option.when(data.nonEmpty) {
          data.map(_.toString).mkString(",") + str
        }
      }
      .map(_.foldLeft(firstPart)(_ + _))
}
