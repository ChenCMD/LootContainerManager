package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import scala.util.chaining.*

import org.bukkit.Location as BukkitLocation
import org.bukkit.World

case class Position(w: World, x: Double, y: Double, z: Double) {
  infix def +(other: Position): Position = Position(w, x + other.x, y + other.y, z + other.z)
  infix def +(vec: Vector): Position     = Position(w, x + vec.x, y + vec.y, z + vec.z)

  infix def -(other: Position): Position = Position(w, x - other.x, y - other.y, z - other.z)
  infix def -(vec: Vector): Position     = Position(w, x - vec.x, y - vec.y, z - vec.z)

  infix def *(other: Position): Position = Position(w, x * other.x, y * other.y, z * other.z)
  infix def *(vec: Vector): Position     = Position(w, x * vec.x, y * vec.y, z * vec.z)

  infix def /(other: Position): Position = Position(w, x / other.x, y / other.y, z / other.z)
  infix def /(vec: Vector): Position     = Position(w, x / vec.x, y / vec.y, z / vec.z)

  infix def *(n: Double): Position = Position(w, x * n, y * n, z * n)

  infix def /(n: Double): Position = Position(w, x / n, y / n, z / n)

  def toVector: Vector = Vector(x, y, z)

  def toBlockLocation: BlockLocation = BlockLocation(w, x.toInt, y.toInt, z.toInt)

  def toBukkit: BukkitLocation = new BukkitLocation(w, x, y, z)
}

object Position {
  def of(l: BukkitLocation): Position = new Position(l.getWorld, l.getX, l.getY, l.getZ)
}
