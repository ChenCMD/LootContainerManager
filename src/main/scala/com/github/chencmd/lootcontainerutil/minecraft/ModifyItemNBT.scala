package com.github.chencmd.lootcontainerutil.minecraft

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.SyncIO

import org.bukkit.inventory.ItemStack

trait ManageItemNBT {
  def createItemFromNBT(compound: NBTTagCompound): SyncIO[ItemStack]
}
