package com.github.chencmd.lootcontainerutil.feature.asset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import cats.data.NonEmptyList

trait LootAssetPersistenceInstr[F[_]] {
  def initialize(): F[Unit] // TODO move migration
  def findLootAsset(location: BlockLocation): F[Option[LootAsset]]
  def getAllLootAssets(): F[List[LootAsset]]
  def upsertLootAsset(lootAsset: LootAsset): F[Unit]
  def upsertLootAssets(lootAssets: List[LootAsset]): F[Unit]
  def deleteLootAsset(asset: LootAsset): F[Unit]
  def deleteLootAssets(ids: NonEmptyList[Int]): F[Unit]
}
