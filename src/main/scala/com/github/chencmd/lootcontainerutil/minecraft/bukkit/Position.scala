package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.generic.extensions.OptionExt.*

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.Bukkit
import org.bukkit.Location as BukkitLocation
import org.bukkit.World

case class Position(world: String, x: Double, y: Double, z: Double) {
  infix def +(other: Position): Position = Position(world, x + other.x, y + other.y, z + other.z)
  infix def +(vec: Vector): Position     = Position(world, x + vec.x, y + vec.y, z + vec.z)

  infix def -(other: Position): Position = Position(world, x - other.x, y - other.y, z - other.z)
  infix def -(vec: Vector): Position     = Position(world, x - vec.x, y - vec.y, z - vec.z)

  infix def *(other: Position): Position = Position(world, x * other.x, y * other.y, z * other.z)
  infix def *(vec: Vector): Position     = Position(world, x * vec.x, y * vec.y, z * vec.z)

  infix def /(other: Position): Position = Position(world, x / other.x, y / other.y, z / other.z)
  infix def /(vec: Vector): Position     = Position(world, x / vec.x, y / vec.y, z / vec.z)

  infix def *(n: Double): Position = Position(world, x * n, y * n, z * n)

  infix def /(n: Double): Position = Position(world, x / n, y / n, z / n)

  def toBukkit(world: World): BukkitLocation  = new BukkitLocation(world, x, y, z)
  def toBukkit[F[_]: Sync]: F[BukkitLocation] = for {
    worldOpt <- Sync[F].delay(Bukkit.getWorlds().asScala.toList.find(_.getKey.toString == world))
    world    <- worldOpt.orRaiseF(SystemException(s"Missing world: $world"))
  } yield new BukkitLocation(world, x, y, z)

  def toBlockLocation: BlockLocation = BlockLocation(world, x.toInt, y.toInt, z.toInt)
  def toVector: Vector               = Vector(x, y, z)

  override def toString(): String = s"$world $x, $y, $z (${world.hashCode})"
}

object Position {
  def of(l: BukkitLocation): Position = new Position(l.getWorld.getKey.toString, l.getX, l.getY, l.getZ)
}
