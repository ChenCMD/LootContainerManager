package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.minecraft.ContainerMeta

import cats.effect.kernel.Sync

import org.bukkit.inventory.Inventory
import org.bukkit.inventory.InventoryHolder

class InventorySession private (
  val id: String,
  val location: BlockLocation,
  val containerMeta: ContainerMeta,
  private val inventoryConstructor: InventoryHolder => Inventory
) extends InventoryHolder {

  private val sessionInventory = inventoryConstructor(this)

  override def getInventory(): Inventory = sessionInventory
}

object InventorySession {
  def apply[F[_]: Sync](id: String, location: BlockLocation, containerMeta: ContainerMeta)(
    inventoryConstructor: InventoryHolder => Inventory
  ): F[InventorySession] = Sync[F].delay {
    new InventorySession(id, location, containerMeta, inventoryConstructor)
  }
}
