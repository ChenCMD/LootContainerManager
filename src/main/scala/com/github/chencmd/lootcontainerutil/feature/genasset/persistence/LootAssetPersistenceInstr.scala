package com.github.chencmd.lootcontainerutil.feature.genasset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.BlockLocation

import cats.mtl.Raise

trait LootAssetPersistenceInstr[F[_]] {
  def initialize()(using R: Raise[F, String]): F[Unit]
  def findLootAsset(location: BlockLocation)(using R: Raise[F, String]): F[Option[LootAsset]]
  def getLootAssets()(using R: Raise[F, String]): F[List[LootAsset]]
  def storeLootAsset(lootAsset: LootAsset)(using R: Raise[F, String]): F[Unit]
  def deleteLootAsset(location: BlockLocation)(using R: Raise[F, String]): F[Unit]
}
