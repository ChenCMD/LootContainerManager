package com.github.chencmd.lootcontainerutil.adapter.database

import com.github.chencmd.lootcontainerutil.feature.genasset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.genasset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.genasset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.minecraft.Location
import cats.implicits.*
import cats.effect.kernel.Async
import doobie.*
import doobie.implicits.*
import org.bukkit.Bukkit
import scala.jdk.CollectionConverters.*
import cats.mtl.Raise

object LootAssetRepository {
  def createInstr[F[_]: Async](transactor: Transactor[F]): LootAssetPersistenceInstr[F] = {
    new LootAssetPersistenceInstr[F] {
      case class LocationRecordRepr(world: String, x: Int, y: Int, z: Int)
      case class LootAssetRecordRepr(
        location: LocationRecordRepr,
        blockId: String,
        facing: Option[String],
        waterlogged: Option[Boolean],
        chestType: Option[String]
      )

      override def initialize()(using R: Raise[F, String]): F[Unit] = {
        val program = for {
          _ <- sql"""|
          |CREATE TABLE IF NOT EXISTS loot_assets (
          |   world       TEXT     NOT NULL,
          |   x           INT      NOT NULL,
          |   y           INT      NOT NULL,
          |   z           INT      NOT NULL,
          |   block_id    TEXT     NOT NULL,
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
        program.transact(transactor).handleErrorWith(e => R.raise(e.getMessage))
      }

      override def findLootAsset(location: Location[Int])(using R: Raise[F, String]): F[Option[LootAsset]] = {
        val program = for {
          queryResult1   <- sql"""|
            |SELECT
            |   block_id,
            |   facing,
            |   waterlogged,
            |   chest_type
            |FROM
            |   loot_assets
            |WHERE
            |   world = ${location.w.getKey.toString}
            |   AND x = ${location.x}
            |   AND y = ${location.y}
            |   AND z = ${location.z}
            |""".stripMargin
            .query[(String, Option[String], Option[Boolean], Option[String])]
            .option
          lootAssetItems <- sql"""|
            |SELECT
            |   slot,
            |   item,
            |   quantity
            |FROM
            |   loot_asset_items
            |WHERE
            |   world = ${location.w.getKey.toString}
            |   AND x = ${location.x}
            |   AND y = ${location.y}
            |   AND z = ${location.z}
            |""".stripMargin
            .query[LootAssetItem]
            .to[List]
        } yield queryResult1.map((LootAsset(location, _, _, _, _, lootAssetItems)).tupled)
        program.transact(transactor).handleErrorWith(e => R.raise(e.getMessage))
      }

      override def getLootAssets()(using R: Raise[F, String]): F[List[LootAsset]] = {
        for {
          queryResult <- sql"""|
            |SELECT
            |   world, x, y, z,
            |   block_id, facing, waterlogged, chest_type,
            |   slot, item, quantity
            |FROM
            |   loot_assets
            |JOIN
            |   loot_asset_items
            |ON
            |   loot_assets.world = loot_asset_items.world
            |   AND loot_assets.x = loot_asset_items.x
            |   AND loot_assets.y = loot_asset_items.y
            |   AND loot_assets.z = loot_asset_items.z
            |""".stripMargin
            .query[(LootAssetRecordRepr, Option[LootAssetItem])]
            .to[List]
            .transact(transactor)
          
        } yield ???
      }

      override def storeLootAsset(lootAsset: LootAsset)(using R: Raise[F, String]): F[Unit] = {
        val program = for {
          _ <- sql"""|
            |INSERT INTO loot_assets
            |VALUES (
            |   ${lootAsset.location.w.getKey.toString},
            |   ${lootAsset.location.x},
            |   ${lootAsset.location.y},
            |   ${lootAsset.location.z},
            |   ${lootAsset.blockId},
            |   ${lootAsset.facing},
            |   ${lootAsset.waterlogged},
            |   ${lootAsset.chestType}
            |)
            |ON CONFLICT (world, x, y, z)
            |DO UPDATE SET
            |   block_id = ${lootAsset.blockId},
            |   facing = ${lootAsset.facing},
            |   waterlogged = ${lootAsset.waterlogged},
            |   chest_type = ${lootAsset.chestType}
            |""".stripMargin.update.run
          _ <- sql"""|
            |DELETE FROM loot_asset_items
            |WHERE
            |   world = ${lootAsset.location.w.getKey.toString}
            |   AND x = ${lootAsset.location.x}
            |   AND y = ${lootAsset.location.y}
            |   AND z = ${lootAsset.location.z}
            |""".stripMargin.update.run
          _ <- lootAsset.items.traverse_ { item =>
            sql"""|
              |INSERT INTO loot_asset_items
              |VALUES (
              |   ${lootAsset.location.w.getKey.toString},
              |   ${lootAsset.location.x},
              |   ${lootAsset.location.y},
              |   ${lootAsset.location.z},
              |   ${item.slot},
              |   ${item.item},
              |   ${item.quantity}
              |)
              |""".stripMargin.update.run
          }
        } yield ()
        program.transact(transactor)
      }

      override def deleteLootAsset(location: Location[Int])(using R: Raise[F, String]): F[Unit] = {
        sql"""|
          |DELETE FROM loot_assets
          |WHERE
          |   world = ${location.w.getKey.toString}
          |   AND x = ${location.x}
          |   AND y = ${location.y}
          |   AND z = ${location.z}
          |""".stripMargin.update.run.transact(transactor).void
      }
    }
  }
}
