package com.github.chencmd.lootcontainerutil.feature.asset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

trait LootAssetPersistenceCacheInstr[F[_]] {
  def askLootAssetLocationsNear(location: BlockLocation): F[List[LootAsset]]

  def askLootAssetLocationAt(location: BlockLocation): F[Option[LootAsset]]

  def askIfLootAssetPresentAt(location: BlockLocation): F[Boolean]

  def updateLootAsset(assets: LootAsset): F[Unit]

  def deleteLootAssetLocationAt(location: BlockLocation): F[Unit]
}
