package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import scala.util.chaining.*

import org.bukkit.World
import org.bukkit.util.Vector as BukkitVector
import org.joml.Vector3f

case class Vector(x: Double, y: Double, z: Double) {
  infix def +(other: Vector): Vector = Vector(x + other.x, y + other.y, z + other.z)

  infix def -(other: Vector): Vector = Vector(x - other.x, y - other.y, z - other.z)

  infix def *(other: Vector): Vector = Vector(x * other.x, y * other.y, z * other.z)

  infix def /(other: Vector): Vector = Vector(x / other.x, y / other.y, z / other.z)

  infix def *(n: Double): Vector = Vector(x * n, y * n, z * n)

  infix def /(n: Double): Vector = Vector(x / n, y / n, z / n)

  def normalize: Vector = {
    Math.sqrt(x * x + y * y + z * z).pipe(mag => Vector(x / mag, y / mag, z / mag))
  }

  def toBukkit: BukkitVector = new BukkitVector(x, y, z)
  def toJomlVector: Vector3f = Vector3f(x.toFloat, y.toFloat, z.toFloat)

  def toPosition(w: World): Position            = Position(w.getKey.toString, x, y, z)
  def toPosition(w: String): Position           = Position(w, x, y, z)
  def toBlockLocation(w: World): BlockLocation  = BlockLocation(w.getKey.toString, x.toInt, y.toInt, z.toInt)
  def toBlockLocation(w: String): BlockLocation = BlockLocation(w, x.toInt, y.toInt, z.toInt)
}

object Vector {
  def of(v: BukkitVector): Vector = Vector(v.getX, v.getY, v.getZ)
}
