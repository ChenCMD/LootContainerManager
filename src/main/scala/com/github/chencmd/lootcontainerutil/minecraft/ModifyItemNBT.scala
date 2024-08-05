package com.github.chencmd.lootcontainerutil.minecraft

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.kernel.Sync

import org.bukkit.inventory.ItemStack

trait ManageItemNBT {
  def createItemFromNBT[F[_]: Sync](compound: NBTTagCompound): F[ItemStack]
}
