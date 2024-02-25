package com.github.chencmd.lootcontainerutil.minecraft

import scala.util.chaining.*

import org.bukkit.Location as BukkitLocation
import org.bukkit.World

case class Location(w: World, x: Double, y: Double, z: Double) {
  def +(other: Vector): Location = Location(w, x + other.x, y + other.y, z + other.z)
  def -(other: Vector): Location = Location(w, x - other.x, y - other.y, z - other.z)
  def *(other: Vector): Location = Location(w, x * other.x, y * other.y, z * other.z)
  def /(other: Vector): Location = Location(w, x / other.x, y / other.y, z / other.z)
  def *(n: Double): Location     = Location(w, x * n, y * n, z * n)
  def /(n: Double): Location     = Location(w, x / n, y / n, z / n)
  def normalize: Location        = Math.sqrt(x * x + y * y + z * z).pipe(mag => Location(w, x / mag, y / mag, z / mag))

  def toBukkit: BukkitLocation = new BukkitLocation(w, x, y, z)
}

object Location {
  def apply(w: World, x: Double, y: Double, z: Double): Location = new Location(w, x, y, z)
  def apply(l: BukkitLocation): Location                         = new Location(l.getWorld, l.getX, l.getY, l.getZ)
}
