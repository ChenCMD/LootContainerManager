package com.github.chencmd.lootcontainerutil.feature.genasset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.BlockLocation

trait LootAssetPersistenceInstr[F[_]] {
  def initialize(): F[Unit]
  def findLootAsset(location: BlockLocation): F[Option[LootAsset]]
  def getLootAssets(): F[List[LootAsset]]
  def storeLootAsset(lootAsset: LootAsset): F[Unit]
  def deleteLootAsset(location: BlockLocation): F[Unit]
}
