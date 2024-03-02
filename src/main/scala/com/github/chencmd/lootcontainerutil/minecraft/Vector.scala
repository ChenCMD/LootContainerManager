package com.github.chencmd.lootcontainerutil.minecraft

import scala.util.chaining.*

import org.bukkit.World
import org.bukkit.util.Vector as BukkitVector

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

  def toLocation(w: World): Position = Position(w, x, y, z)

  def toBukkit: BukkitVector = new BukkitVector(x, y, z)
}

object Vector {
  def of(v: BukkitVector): Vector = Vector(v.getX, v.getY, v.getZ)
}
