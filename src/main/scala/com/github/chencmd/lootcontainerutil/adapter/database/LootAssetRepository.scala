package com.github.chencmd.lootcontainerutil.adapter.database

import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import doobie.*
import doobie.implicits.*
import org.bukkit.Bukkit
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

object LootAssetRepository {
  def createInstr[F[_]: Async](transactor: Transactor[F]): LootAssetPersistenceInstr[F] = {
    case class LocationRecordRepr(world: String, x: Int, y: Int, z: Int)
    case class LootAssetRecordRepr(
      location: LocationRecordRepr,
      blockId: String,
      name: Option[String],
      facing: Option[String],
      waterlogged: Option[Boolean],
      chestType: Option[String]
    )

    def locationToRepr(location: BlockLocation): LocationRecordRepr = LocationRecordRepr(
      location.w.getKey.toString,
      location.x,
      location.y,
      location.z
    )

    def lootAssetToRepr(lootAsset: LootAsset): (LootAssetRecordRepr, List[LootAssetItem]) = {
      val repr = LootAssetRecordRepr(
        LocationRecordRepr(
          lootAsset.location.w.getKey.toString,
          lootAsset.location.x,
          lootAsset.location.y,
          lootAsset.location.z
        ),
        lootAsset.blockId,
        lootAsset.name,
        lootAsset.facing.map(_.toString.toLowerCase),
        lootAsset.waterlogged,
        lootAsset.chestType.map(_.toString.toLowerCase)
      )
      (repr, lootAsset.items)
    }

    def reprToLootAsset(repr: LootAssetRecordRepr, items: List[LootAssetItem]): F[LootAsset] = for {
      worldOpt  <- Async[F].delay(Bukkit.getWorlds().asScala.toList.find(_.getKey.toString == repr.location.world))
      world     <- worldOpt.fold(SystemException.raise(s"Missing world: ${repr.location.world}"))(_.pure[F])
      facing    <- Either
        .catchNonFatal(repr.facing.map(BlockFace.valueOf))
        .fold(
          _ => SystemException.raise(s"Invalid block face: ${repr.facing}"),
          _.pure[F]
        )
      chestType <- Either
        .catchNonFatal(repr.chestType.map(Chest.Type.valueOf))
        .fold(
          _ => SystemException.raise(s"Invalid chest type: ${repr.chestType}"),
          _.pure[F]
        )
      lootAsset = LootAsset(
        BlockLocation(world, repr.location.x, repr.location.y, repr.location.z),
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
        |   world, x, y, z,
        |   block_id, name, facing, waterlogged, chest_type,
        |   slot, item, quantity
        |FROM
        |   loot_assets
        |LEFT JOIN
        |   loot_asset_items
        |ON
        |   loot_assets.world = loot_asset_items.world
        |   AND loot_assets.x = loot_asset_items.x
        |   AND loot_assets.y = loot_asset_items.y
        |   AND loot_assets.z = loot_asset_items.z
        |""".stripMargin
      val ASSET_DELETE_QUERY = "DELETE FROM loot_assets WHERE world = ? AND x = ? AND y = ? AND z = ?"
      val ITEMS_DELETE_QUERY = "DELETE FROM loot_asset_items WHERE world = ? AND x = ? AND y = ? AND z = ?"

      def makeAssetUpsertQuery(assetRepr: LootAssetRecordRepr): Fragment = {
        sql"INSERT INTO loot_assets VALUES (" ++ Fragments.values(assetRepr) ++ fr") ON CONFLICT REPLACE"
      }

      def makeItemsInsertQuery(items: NonEmptyList[LootAssetItem]): Fragment = {
        sql"INSERT INTO loot_asset_items" ++ Fragments.values(items)
      }

      override def initialize(): F[Unit] = {
        val program = for {
          _ <- sql"""|
          |CREATE TABLE IF NOT EXISTS loot_assets (
          |   world       TEXT     NOT NULL,
          |   x           INT      NOT NULL,
          |   y           INT      NOT NULL,
          |   z           INT      NOT NULL,
          |   block_id    TEXT     NOT NULL,
          |   name        TEXT,
          |   facing      TEXT,
          |   waterlogged BOOLEAN,
          |   chest_type  TEXT,
          |   PRIMARY KEY (world, x, y, z)
          |)""".stripMargin.update.run
          _ <- sql"""|
          |CREATE TABLE IF NOT EXISTS loot_asset_items (
          |   world       TEXT     NOT NULL,
          |   x           INT      NOT NULL,
          |   y           INT      NOT NULL,
          |   z           INT      NOT NULL,
          |   slot        INT      NOT NULL,
          |   item        TEXT     NOT NULL,
          |   quantity    INT      NOT NULL,
          |   PRIMARY KEY (world, x, y, z, slot),
          |   FOREIGN KEY (world, x, y, z) REFERENCES loot_assets (world, x, y, z) ON DELETE CASCADE
          |)""".stripMargin.update.run
        } yield ()
        program.transact(transactor)
      }

      override def findLootAsset(location: BlockLocation): F[Option[LootAsset]] = for {
        whereFr     <- Async[F].pure {
          Fragments.whereAnd(
            fr"loot_assets.world = ${location.w.getKey.toString}",
            fr"loot_assets.x = ${location.x}",
            fr"loot_assets.y = ${location.y}",
            fr"loot_assets.z = ${location.z}"
          )
        }
        queryResult <- (ASSET_SELECT_QUERY ++ whereFr)
          .query[(LootAssetRecordRepr, Option[LootAssetItem])]
          .option
          .transact(transactor)
        lootAsset   <- queryResult.traverse((repr, item) => reprToLootAsset(repr, item.toList))
      } yield lootAsset

      override def getLootAssets(): F[List[LootAsset]] = for {
        queryResult <- ASSET_SELECT_QUERY
          .query[(LootAssetRecordRepr, Option[LootAssetItem])]
          .to[List]
          .transact(transactor)
        lootAsset   <- queryResult.traverse((repr, item) => reprToLootAsset(repr, item.toList))
      } yield lootAsset

      override def storeLootAsset(lootAsset: LootAsset): F[Unit] = {
        val (assetRepr, items) = lootAssetToRepr(lootAsset)
        val program            = for {
          _ <- makeAssetUpsertQuery(assetRepr).update.run
          _ <- Update[LocationRecordRepr](ITEMS_DELETE_QUERY).run(assetRepr.location)
          _ <- items.toNel.traverse(nel => makeItemsInsertQuery(nel).update.run)
        } yield ()
        program.transact(transactor)
      }

      override def deleteLootAsset(location: BlockLocation): F[Unit] = {
        Update[LocationRecordRepr](ITEMS_DELETE_QUERY)
          .run(locationToRepr(location))
          .transact(transactor)
          .void
      }
    }
  }
}
