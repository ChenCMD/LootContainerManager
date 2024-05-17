package com.github.chencmd.lootcontainerutil.feature.asset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

trait LootAssetPersistenceInstr[F[_]] {
  def initialize(): F[Unit] // TODO move migration
  def findLootAsset(location: BlockLocation): F[Option[LootAsset]]
  def getAllLootAssets(): F[List[LootAsset]]
  def upsertLootAsset(lootAsset: LootAsset): F[Unit]
  def deleteLootAsset(location: BlockLocation): F[Unit]
}
