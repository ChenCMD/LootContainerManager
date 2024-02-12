package com.github.chencmd.lootcontainerutil.minecraft

import scala.util.chaining.*

import org.bukkit.util.Vector as BukkitVector

case class Vector(x: Double, y: Double, z: Double) {
  def +(other: Vector): Vector = Vector(x + other.x, y + other.y, z + other.z)
  def -(other: Vector): Vector = Vector(x - other.x, y - other.y, z - other.z)
  def *(other: Vector): Vector = Vector(x * other.x, y * other.y, z * other.z)
  def /(other: Vector): Vector = Vector(x / other.x, y / other.y, z / other.z)
  def *(n: Double): Vector     = Vector(x * n, y * n, z * n)
  def /(n: Double): Vector     = Vector(x / n, y / n, z / n)
  def normalize: Vector        = Math.sqrt(x * x + y * y + z * z).pipe(mag => Vector(x / mag, y / mag, z / mag))

  def toBukkit: BukkitVector = new BukkitVector(x, y, z)
}

object Vector {
  def apply(v: BukkitVector): Vector = Vector(v.getX, v.getY, v.getZ)
}
