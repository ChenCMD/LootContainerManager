package com.github.chencmd.lootcontainerutil.minecraft

import scala.math.Fractional.Implicits.*
import scala.math.Integral.Implicits.*
import scala.math.Numeric.Implicits.*
import scala.util.chaining.*

import org.bukkit.World
import org.bukkit.util.Vector as BukkitVector

case class Vector[A: Numeric](x: A, y: A, z: A) {
  infix def +(other: Vector[A]): Vector[A]                         = Vector(x + other.x, y + other.y, z + other.z)

  infix def -(other: Vector[A]): Vector[A]                         = Vector(x - other.x, y - other.y, z - other.z)

  infix def *(other: Vector[A]): Vector[A]                         = Vector(x * other.x, y * other.y, z * other.z)

  infix def /(other: Vector[A])(using I: Integral[A]): Vector[A]   = Vector(x / other.x, y / other.y, z / other.z)
  infix def /(other: Vector[A])(using F: Fractional[A]): Vector[A] = Vector(x / other.x, y / other.y, z / other.z)

  infix def *(n: A): Vector[A]                                     = Vector(x * n, y * n, z * n)

  infix def /(n: A)(using I: Integral[A]): Vector[A]               = Vector(x / n, y / n, z / n)
  infix def /(n: A)(using F: Fractional[A]): Vector[A]             = Vector(x / n, y / n, z / n)

  def normalize: Vector[Double]                                    = {
    val dx = x.toDouble
    val dy = y.toDouble
    val dz = z.toDouble
    Math.sqrt(dx * dx + dy * dy + dz * dz).pipe(mag => Vector(dx / mag, dy / mag, dz / mag))
  }

  def toLocation(w: World): Location[A] = Location(w, x, y, z)

  def toBukkit: BukkitVector = new BukkitVector(x.toDouble, y.toDouble, z.toDouble)
}

object Vector {
  def apply[A: Numeric](x: A, y: A, z: A): Vector[A] = new Vector(x, y, z)
  def of(v: BukkitVector): Vector[Double]            = Vector(v.getX, v.getY, v.getZ)
  def ofInt(v: BukkitVector): Vector[Int]            = Vector(v.getX.toInt, v.getY.toInt, v.getZ.toInt)
}
