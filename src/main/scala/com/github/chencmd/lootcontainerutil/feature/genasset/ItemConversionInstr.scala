package com.github.chencmd.lootcontainerutil.feature.genasset

import org.bukkit.inventory.ItemStack

type ItemIdentifier = String

trait ItemConversionInstr[F[_]] {
  def toItemIdentifier(item: ItemStack): F[ItemIdentifier]
  def toItemStack(itemIdentifier: ItemIdentifier): F[ItemStack]
}
