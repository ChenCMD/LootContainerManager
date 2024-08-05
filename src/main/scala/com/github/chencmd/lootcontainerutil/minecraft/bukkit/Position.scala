package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.exceptions.SystemException

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.Bukkit
import org.bukkit.Location as BukkitLocation
import org.bukkit.World

case class Position(w: String, x: Double, y: Double, z: Double) {
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

  def toBukkit(world: World): BukkitLocation  = new BukkitLocation(world, x, y, z)
  def toBukkit[F[_]: Sync]: F[BukkitLocation] = for {
    worldOpt <- Sync[F].delay(Bukkit.getWorlds().asScala.toList.find(_.getKey.toString == w))
    world    <- worldOpt.fold(SystemException.raise(s"Missing world: $w"))(_.pure[F])
  } yield new BukkitLocation(world, x, y, z)

  def toBlockLocation: BlockLocation = BlockLocation(w, x.toInt, y.toInt, z.toInt)
  def toVector: Vector               = Vector(x, y, z)

  override def toString(): String = s"$w $x, $y, $z (${w.hashCode})"
}

object Position {
  def of(l: BukkitLocation): Position = new Position(l.getWorld.getKey.toString, l.getX, l.getY, l.getZ)
}
