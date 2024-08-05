package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagCompound

import cats.effect.kernel.Sync
import cats.implicits.*

import org.bukkit.inventory.ItemStack

object ManageBukkitItemNBT {
  def createInstr: ManageItemNBT = new ManageItemNBT {
    import CraftItemStack.*

    def createItemFromNBT[F[_]: Sync](compound: NBTTagCompound): F[ItemStack] = for {
      nmsItem   <- NMSItemStack(compound)
      craftItem <- CraftItemStack.asBukkitCopy(nmsItem)
    } yield craftItem
  }
}
