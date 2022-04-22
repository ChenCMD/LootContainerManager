package com.github.chencmd.lootcontainerutil.generic.givens

import java.util.UUID

import org.bukkit.{GameMode, Material}
import org.bukkit.event.block.Action
import org.bukkit.event.inventory.InventoryType
import org.bukkit.inventory.ItemStack
import com.comphenix.protocol.wrappers.EnumWrappers.ItemSlot

object CanEquals {

  // Java Classes
  given CanEqual[UUID, UUID] = CanEqual.derived


  // Bukkit Classes
  given CanEqual[Material, Material] = CanEqual.derived

  given CanEqual[Action, Action] = CanEqual.derived

  given CanEqual[InventoryType, InventoryType] = CanEqual.derived

  given CanEqual[ItemStack, ItemStack] = CanEqual.derived

  given CanEqual[GameMode, GameMode] = CanEqual.derived


  // ProtocolLib Classes
  given CanEqual[ItemSlot, ItemSlot] = CanEqual.derived
}