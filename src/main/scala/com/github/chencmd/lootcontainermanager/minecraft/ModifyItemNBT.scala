package com.github.chencmd.lootcontainermanager.minecraft

import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.kernel.Sync

import org.bukkit.inventory.ItemStack

trait ManageItemNBT {
  def createItemFromNBT[F[_]: Sync](compound: NBTTagCompound): F[ItemStack]
}
