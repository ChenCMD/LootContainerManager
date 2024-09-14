package com.github.chencmd.lootcontainermanager.minecraft

import org.bukkit.DyeColor
import org.bukkit.Sound
import org.bukkit.event.inventory.InventoryType

enum ContainerMeta(
  val id: Set[String],
  val inventoryType: InventoryType,
  val openedSound: Option[Sound] = None,
  val closedSound: Option[Sound] = None
) {
  case Barrel
      extends ContainerMeta(
        Set("barrel"),
        InventoryType.BARREL,
        Some(Sound.BLOCK_BARREL_OPEN),
        Some(Sound.BLOCK_BARREL_CLOSE)
      )
  case BlastFurnace extends ContainerMeta(Set("blast_furnace"), InventoryType.BLAST_FURNACE)
  case BrewingStand extends ContainerMeta(Set("brewing_stand"), InventoryType.BREWING)
  case Chest
      extends ContainerMeta(
        Set("chest", "trapped_chest"),
        InventoryType.CHEST,
        Some(Sound.BLOCK_CHEST_OPEN),
        Some(Sound.BLOCK_CHEST_CLOSE)
      )
  case Dispenser    extends ContainerMeta(Set("dispenser"), InventoryType.DISPENSER)
  case Dropper      extends ContainerMeta(Set("dropper"), InventoryType.DROPPER)
  case Furnace      extends ContainerMeta(Set("furnace"), InventoryType.FURNACE)
  case Hopper       extends ContainerMeta(Set("hopper"), InventoryType.HOPPER)
  case ShulkerBox
      extends ContainerMeta(
        Set.from(DyeColor.values().map(d => s"${d.toString.toLowerCase}_shulker_box") :+ "shulker_box"),
        InventoryType.SHULKER_BOX,
        Some(Sound.BLOCK_SHULKER_BOX_OPEN),
        Some(Sound.BLOCK_SHULKER_BOX_CLOSE)
      )
  case Smoker       extends ContainerMeta(Set("smoker"), InventoryType.SMOKER)
}

object ContainerMeta {
  def fromId(id: String): Option[ContainerMeta] = values.find(_.id.contains(id.toLowerCase))
}
