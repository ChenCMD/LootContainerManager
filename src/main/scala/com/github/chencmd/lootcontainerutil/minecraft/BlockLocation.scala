package com.github.chencmd.lootcontainerutil.minecraft

import org.bukkit.Location as BukkitLocation
import org.bukkit.World

final case class BlockLocation(w: World, x: Int, y: Int, z: Int) {
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

  def toBukkit: BukkitLocation = new BukkitLocation(w, x, y, z)
  def toPosition: Position          = Position(w, x.toDouble, y.toDouble, z.toDouble)
  def toVector: Vector              = Vector(x.toDouble, y.toDouble, z.toDouble)
}

object BlockLocation {
  def of(l: BukkitLocation): BlockLocation = new BlockLocation(l.getWorld, l.getBlockX, l.getBlockY, l.getBlockZ)
}
