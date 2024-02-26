package com.github.chencmd.lootcontainerutil.feature.genasset

import org.bukkit.inventory.ItemStack
import cats.mtl.Raise

type ItemIdentifier = String

trait ItemConversionInstr[F[_]] {
  def toItemIdentifier(item: ItemStack)(using R: Raise[F, String]): F[ItemIdentifier]
  def toItemStack(itemIdentifier: ItemIdentifier)(using R: Raise[F, String]): F[ItemStack]
}
