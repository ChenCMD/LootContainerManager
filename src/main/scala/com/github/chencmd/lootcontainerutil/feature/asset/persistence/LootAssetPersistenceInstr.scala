package com.github.chencmd.lootcontainermanager.feature.asset.persistence

import cats.data.NonEmptyList

trait LootAssetPersistenceInstr[F[_]] {
  def getAllLootAssets(): F[List[LootAsset]]
  def upsertLootAssets(lootAssets: List[LootAsset]): F[Unit]
  def deleteLootAssets(ids: NonEmptyList[Int]): F[Unit]
}
