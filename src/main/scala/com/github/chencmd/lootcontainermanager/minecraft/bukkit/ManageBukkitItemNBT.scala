package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainermanager.minecraft.nms.NMSItemStack
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.kernel.Sync
import cats.implicits.*

import org.bukkit.inventory.ItemStack

object ManageBukkitItemNBT {
  def createInstr: ManageItemNBT = new ManageItemNBT {
    def createItemFromNBT[F[_]: Sync](compound: NBTTagCompound): F[ItemStack] = for {
      nmsItem   <- NMSItemStack(compound)
      craftItem <- CraftItemStack.asBukkitCopy(nmsItem)
    } yield craftItem
  }
}
