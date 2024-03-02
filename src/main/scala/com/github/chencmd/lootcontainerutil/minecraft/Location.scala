package com.github.chencmd.lootcontainerutil.minecraft

import scala.math.Fractional.Implicits.*
import scala.math.Integral.Implicits.*
import scala.math.Numeric.Implicits.*
import scala.util.chaining.*

import org.bukkit.Location as BukkitLocation
import org.bukkit.World

case class Location[A: Numeric](w: World, x: A, y: A, z: A) {
  infix def +(other: Location[A]): Location[A] = Location(w, x + other.x, y + other.y, z + other.z)
  infix def +(vec: Vector[A]): Location[A]     = Location(w, x + vec.x, y + vec.y, z + vec.z)

  infix def -(other: Location[A]): Location[A] = Location(w, x - other.x, y - other.y, z - other.z)
  infix def -(vec: Vector[A]): Location[A]     = Location(w, x - vec.x, y - vec.y, z - vec.z)

  infix def *(other: Location[A]): Location[A] = Location(w, x * other.x, y * other.y, z * other.z)
  infix def *(vec: Vector[A]): Location[A]     = Location(w, x * vec.x, y * vec.y, z * vec.z)

  infix def /(other: Location[A])(using I: Integral[A]): Location[A]   = Location(w, x / other.x, y / other.y, z / other.z)
  infix def /(other: Location[A])(using F: Fractional[A]): Location[A] = Location(w, x / other.x, y / other.y, z / other.z)
  infix def /(vec: Vector[A])(using I: Integral[A]): Location[A]       = Location(w, x / vec.x, y / vec.y, z / vec.z)
  infix def /(vec: Vector[A])(using F: Fractional[A]): Location[A]     = Location(w, x / vec.x, y / vec.y, z / vec.z)

  infix def *(n: A): Location[A] = Location(w, x * n, y * n, z * n)

  infix def /(n: A)(using I: Integral[A]): Location[A]   = Location(w, x / n, y / n, z / n)
  infix def /(n: A)(using F: Fractional[A]): Location[A] = Location(w, x / n, y / n, z / n)

  def normalize: Location[Double] = {
    val dx = x.toDouble
    val dy = y.toDouble
    val dz = z.toDouble
    Math.sqrt(dx * dx + dy * dy + dz * dz).pipe(mag => Location(w, dx / mag, dy / mag, dz / mag))
  }

  def toVector: Vector[A] = Vector(x, y, z)

  def toBukkit: BukkitLocation = new BukkitLocation(w, x.toDouble, y.toDouble, z.toDouble)
}

object Location {
  def apply[A: Numeric](w: World, x: A, y: A, z: A): Location[A] = new Location(w, x, y, z)
  def of(l: BukkitLocation): Location[Double]                    = new Location(l.getWorld, l.getX, l.getY, l.getZ)
  def ofInt(l: BukkitLocation): Location[Int] = new Location(l.getWorld, l.getX.toInt, l.getY.toInt, l.getZ.toInt)
}
