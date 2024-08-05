package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import cats.effect.kernel.Sync

import org.bukkit.inventory.Inventory
import org.bukkit.inventory.InventoryHolder

class InventorySession private (
  val id: String,
  val location: BlockLocation,
  private val inventoryConstructor: InventoryHolder => Inventory
) extends InventoryHolder {

  private val sessionInventory = inventoryConstructor(this)

  override def getInventory(): Inventory = sessionInventory
}

object InventorySession {
  def apply[F[_]: Sync](id: String, location: BlockLocation)(
    inventoryConstructor: InventoryHolder => Inventory
  ): F[InventorySession] = Sync[F].delay {
    new InventorySession(id, location, inventoryConstructor)
  }
}
