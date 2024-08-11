package com.github.chencmd.lootcontainerutil.adapter.database

import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.generic.FragmentsExtra
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.implicits.*

import com.github.tarao.record4s.ArrayRecord
import doobie.*
import doobie.implicits.*
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

object LootAssetRepository {
  def createInstr[F[_]: Async](transactor: Transactor[F]): LootAssetPersistenceInstr[F] = {
    type LocationRecordRepr      = ArrayRecord[
      (
        ("world", String),
        ("x", Int),
        ("y", Int),
        ("z", Int)
      )
    ]
    type LootAssetRecordRepr     = ArrayRecord[
      (
        ("id", Option[Int]),
        ("world", String),
        ("x", Int),
        ("y", Int),
        ("z", Int),
        ("blockId", String),
        ("name", Option[String]),
        ("facing", Option[String]),
        ("waterlogged", Option[Boolean]),
        ("chestType", Option[String])
      )
    ]
    type LootAssetItemRecordRepr = ArrayRecord[
      (
        ("id", Option[Int]),
        ("slot", Int),
        ("item", String),
        ("quantity", Int)
      )
    ]

    def lootAssetItemToRepr(location: BlockLocation)(item: LootAssetItem): LootAssetItemRecordRepr = ArrayRecord(
      id = None: Option[Int],
      slot = item.slot,
      item = item.item,
      quantity = item.quantity
    )

    def lootAssetToRepr(
      lootAsset: LootAsset
    ): (LocationRecordRepr, LootAssetRecordRepr, List[LootAssetItemRecordRepr]) = {
      val location = ArrayRecord(
        world = lootAsset.location.w,
        x = lootAsset.location.x,
        y = lootAsset.location.y,
        z = lootAsset.location.z
      )
      val repr     = ArrayRecord(id = None: Option[Int])
        ++ location
        + (
          blockId = lootAsset.blockId,
          name = lootAsset.name,
          facing = lootAsset.facing.map(_.toString.toLowerCase),
          waterlogged = lootAsset.waterlogged,
          chestType = lootAsset.chestType.map(_.toString.toLowerCase)
        )
      (location, repr, lootAsset.items.map(lootAssetItemToRepr(lootAsset.location)))
    }

    def reprToLootAsset(repr: LootAssetRecordRepr, items: List[LootAssetItem]): F[LootAsset] = for {
      facing    <- Either
        .catchNonFatal(repr.facing.map(_.toUpperCase).map(BlockFace.valueOf))
        .fold(_ => SystemException.raise(s"Invalid block face: ${repr.facing}"), _.pure[F])
      chestType <- Either
        .catchNonFatal(repr.chestType.map(_.toUpperCase).map(Chest.Type.valueOf))
        .fold(_ => SystemException.raise(s"Invalid chest type: ${repr.chestType}"), _.pure[F])
      lootAsset = LootAsset(
        repr.id,
        BlockLocation(repr.world, repr.x, repr.y, repr.z),
        repr.blockId,
        repr.name,
        facing,
        repr.waterlogged,
        chestType,
        items
      )
    } yield lootAsset

    new LootAssetPersistenceInstr[F] {
      val ASSET_SELECT_QUERY = sql"""|
        |SELECT
        |   asset.id, asset.world, asset.x, asset.y, asset.z,
        |   asset.block_id, asset.name, asset.facing, asset.waterlogged, asset.chest_type,
        |   item.slot, item.item, item.quantity
        |FROM
        |   loot_assets AS asset
        |LEFT JOIN
        |   loot_asset_items AS item
        |ON
        |   asset.id = item.asset_id
        |""".stripMargin

      def makeAssetUpsertQuery(assetRepr: LootAssetRecordRepr): Fragment = {
        sql"INSERT OR REPLACE INTO loot_assets VALUES ${FragmentsExtra.tupled(assetRepr.values)}"
      }

      def makeAssetsDeleteQuery(assetIds: NonEmptyList[Int]): Fragment = {
        sql"DELETE FROM loot_assets WHERE ${Fragments.in(fr"id", assetIds)}"
      }

      def makeItemsInsertQuery(assetId: Int, items: NonEmptyList[LootAssetItemRecordRepr]): Fragment = {
        val a = items.map(ArrayRecord(asset_id = assetId) ++ _)
        sql"INSERT INTO loot_asset_items(asset_id, id, slot, item, quantity) ${Fragments.values(a)}"
      }

      def makeItemsDeleteQuery(assetIds: NonEmptyList[Int]): Fragment = {
        sql"DELETE FROM loot_asset_items WHERE ${Fragments.in(fr"asset_id", assetIds)}"
      }

      override def initialize(): F[Unit] = {
        val program = for {
          // TODO ちゃんと Migration を用意する
          _ <- sql"""|
          |CREATE TABLE IF NOT EXISTS loot_assets (
          |   id          INTEGER  PRIMARY KEY AUTOINCREMENT,
          |   world       TEXT     NOT NULL,
          |   x           INT      NOT NULL,
          |   y           INT      NOT NULL,
          |   z           INT      NOT NULL,
          |   block_id    TEXT     NOT NULL,
          |   name        TEXT,
          |   facing      TEXT,
          |   waterlogged BOOLEAN,
          |   chest_type  TEXT,
          |   UNIQUE (world, x, y, z)
          |)""".stripMargin.update.run
          _ <- sql"""|
          |CREATE TABLE IF NOT EXISTS loot_asset_items (
          |   id          INTEGER  PRIMARY KEY AUTOINCREMENT,
          |   asset_id    INT      NOT NULL,
          |   slot        INT      NOT NULL,
          |   item        TEXT     NOT NULL,
          |   quantity    INT      NOT NULL,
          |   FOREIGN KEY (asset_id) REFERENCES loot_assets (id)
          |   UNIQUE (asset_id, slot)
          |)""".stripMargin.update.run
        } yield ()
        program.transact(transactor)
      }

      override def getAllLootAssets(): F[List[LootAsset]] = for {
        queryResult <- ASSET_SELECT_QUERY
          .query[(LootAssetRecordRepr, Option[LootAssetItem])]
          .to[List]
          .transact(transactor)
        lootAsset   <- queryResult.groupMap(_._1)(_._2).toList.traverse {
          case (repr, item) => reprToLootAsset(repr, item.flatten)
        }
      } yield lootAsset

      override def upsertLootAssets(lootAssets: List[LootAsset]): F[Unit] = {
        lootAssets.toNel.traverse_(_.traverse_(createUpsertLootAssetTransaction)).transact(transactor)
      }

      override def deleteLootAssets(ids: NonEmptyList[Int]): F[Unit] = {
        val program = for {
          _ <- makeItemsDeleteQuery(ids).update.run
          _ <- makeAssetsDeleteQuery(ids).update.run
        } yield ()
        program.transact(transactor)
      }

      private def createUpsertLootAssetTransaction(lootAsset: LootAsset): ConnectionIO[Unit] = {
        val (location, assetRepr, items) = lootAssetToRepr(lootAsset)
        for {
          _  <- lootAsset.id.traverse(id => makeItemsDeleteQuery(NonEmptyList.one(id)).update.run)
          id <- makeAssetUpsertQuery(assetRepr).update.withUniqueGeneratedKeys[Int]("id")
          _  <- items.toNel.traverse(makeItemsInsertQuery(id, _).update.run)
        } yield ()
      }
    }
  }
}
