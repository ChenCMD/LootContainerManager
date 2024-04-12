package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.SyncIO

import org.bukkit.inventory.ItemStack

object ManageBukkitItemNBT {
  def createInstr: ManageItemNBT = new ManageItemNBT {
    import CraftItemStack.*

    def createItemFromNBT(compound: NBTTagCompound): SyncIO[ItemStack] = for {
      nmsItem   <- NMSItemStack(compound)
      craftItem <- CraftItemStack.asCraftMirror(nmsItem)
    } yield craftItem
  }
}
