package com.github.chencmd.lootcontainerutil.feature.genasset

import cats.mtl.Raise

import org.bukkit.inventory.ItemStack

type ItemIdentifier = String

trait ItemConversionInstr[F[_]] {
  def toItemIdentifier(item: ItemStack)(using R: Raise[F, String]): F[ItemIdentifier]
  def toItemStack(itemIdentifier: ItemIdentifier)(using R: Raise[F, String]): F[ItemStack]
}
