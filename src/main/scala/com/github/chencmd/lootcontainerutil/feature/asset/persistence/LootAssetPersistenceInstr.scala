package com.github.chencmd.lootcontainerutil.feature.asset.persistence

import cats.data.NonEmptyList

trait LootAssetPersistenceInstr[F[_]] {
  def initialize(): F[Unit] // TODO move migration
  def getAllLootAssets(): F[List[LootAsset]]
  def upsertLootAssets(lootAssets: List[LootAsset]): F[Unit]
  def deleteLootAssets(ids: NonEmptyList[Int]): F[Unit]
}
