package com.github.chencmd.lootcontainerutil.adapter.database

import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetContainer
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.generic.FragmentsExtra
import com.github.chencmd.lootcontainerutil.generic.instances.MetaInstances.given
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.implicits.*

import com.github.tarao.record4s.ArrayRecord
import doobie.*
import doobie.implicits.*
import java.util.UUID
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

object LootAssetRepository {
  def createInstr[F[_]: Async](transactor: Transactor[F]): LootAssetPersistenceInstr[F] = {
    type LocationRecordRepr           = ArrayRecord[
      (
        ("world", String),
        ("x", Int),
        ("y", Int),
        ("z", Int)
      )
    ]
    type LootAssetRecordRepr          = ArrayRecord[
      (
        ("id", Option[Int]),
        ("uuid", UUID),
        ("name", Option[String])
      )
    ]
    type LootAssetContainerRecordRepr = ArrayRecord[
      (
        ("location", LocationRecordRepr),
        ("blockId", String),
        ("facing", Option[String]),
        ("waterlogged", Option[Boolean]),
        ("chestType", Option[String])
      )
    ]
    type LootAssetItemRecordRepr      = ArrayRecord[
      (
        ("slot", Int),
        ("item", String),
        ("quantity", Int)
      )
    ]

    def locationToRepr(location: BlockLocation): LocationRecordRepr = {
      ArrayRecord(world = location.world, x = location.x, y = location.y, z = location.z)
    }

    def lootAssetContainerToRepr(container: LootAssetContainer): LootAssetContainerRecordRepr = ArrayRecord(
      location = locationToRepr(container.location),
      blockId = container.blockId,
      facing = container.facing.map(_.name),
      waterlogged = container.waterlogged,
      chestType = container.chestType.map(_.name)
    )

    def lootAssetItemToRepr(item: LootAssetItem): LootAssetItemRecordRepr = ArrayRecord(
      slot = item.slot,
      item = item.item,
      quantity = item.quantity
    )

    def lootAssetToRepr(
      lootAsset: LootAsset
    ): (LootAssetRecordRepr, List[LootAssetContainerRecordRepr], List[LootAssetItemRecordRepr]) = {
      val assetRepr  = ArrayRecord(id = lootAsset.id, uuid = lootAsset.uuid, name = lootAsset.name)
      val containers = lootAsset.containers.map(lootAssetContainerToRepr)
      val items      = lootAsset.items.map(lootAssetItemToRepr)
      (assetRepr, containers, items)
    }

    def reprToLootAssetContainer(repr: LootAssetContainerRecordRepr): F[LootAssetContainer] = for {
      facing    <- Either
        .catchNonFatal(repr.facing.map(_.toUpperCase).map(BlockFace.valueOf))
        .fold(_ => SystemException.raise(s"Invalid block face: ${repr.facing}"), _.pure[F])
      chestType <- Either
        .catchNonFatal(repr.chestType.map(_.toUpperCase).map(Chest.Type.valueOf))
        .fold(_ => SystemException.raise(s"Invalid chest type: ${repr.chestType}"), _.pure[F])
      lootAsset = LootAssetContainer(
        BlockLocation(repr.location.world, repr.location.x, repr.location.y, repr.location.z),
        repr.blockId,
        facing,
        repr.waterlogged,
        chestType
      )
    } yield lootAsset

    def reprToLootAsset(
      recordRepr: LootAssetRecordRepr,
      recordReprContainers: List[LootAssetContainerRecordRepr],
      recordReprItems: List[LootAssetItemRecordRepr]
    ): F[LootAsset] = for {
      containers <- recordReprContainers.traverse(reprToLootAssetContainer)
      items = recordReprItems.map(repr => LootAssetItem(repr.slot, repr.item, repr.quantity))
      asset = LootAsset(
        id = recordRepr.id,
        uuid = recordRepr.uuid,
        name = recordRepr.name,
        containers = containers,
        items = items
      )
    } yield asset

    new LootAssetPersistenceInstr[F] {
      val ASSET_CREATE_QUERIES = List(
        // TODO ちゃんと Migration を用意する
        sql"""|CREATE TABLE IF NOT EXISTS loot_assets (
              |   id          INTEGER  PRIMARY KEY AUTOINCREMENT,
              |   uuid        TEXT     NOT NULL,
              |   name        TEXT,
              |
              |   UNIQUE (uuid)
              |)""".stripMargin,
        sql"""|CREATE TABLE IF NOT EXISTS loot_asset_containers (
              |   id          INTEGER  PRIMARY KEY AUTOINCREMENT,
              |   asset_id    INT      NOT NULL,
              |   world       TEXT     NOT NULL,
              |   x           INT      NOT NULL,
              |   y           INT      NOT NULL,
              |   z           INT      NOT NULL,
              |   block_id    TEXT     NOT NULL,
              |   facing      TEXT,
              |   waterlogged BOOLEAN,
              |   chest_type  TEXT,
              |
              |   FOREIGN KEY (asset_id) REFERENCES loot_assets (id),
              |   UNIQUE (asset_id, world, x, y, z)
              |)""".stripMargin,
        sql"""|CREATE TABLE IF NOT EXISTS loot_asset_items (
              |   id          INTEGER  PRIMARY KEY AUTOINCREMENT,
              |   asset_id    INT      NOT NULL,
              |   slot        INT      NOT NULL,
              |   item        TEXT     NOT NULL,
              |   quantity    INT      NOT NULL,
              |
              |   FOREIGN KEY (asset_id) REFERENCES loot_assets (id)
              |   UNIQUE (asset_id, slot)
              |)""".stripMargin
      )

      val ASSET_SELECT_QUERY = sql"""|
        |SELECT
        |   asset.id, asset.uuid, asset.name,
        |   container.world, container.x, container.y, container.z,
        |   container.block_id, container.facing, container.waterlogged, container.chest_type,
        |   item.slot, item.item, item.quantity
        |FROM
        |   loot_assets AS asset
        |LEFT JOIN
        |  loot_asset_containers AS container
        |ON
        |   asset.id = container.asset_id
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

      def makeContainersInsertQuery(assetId: Int, containers: NonEmptyList[LootAssetContainerRecordRepr]): Fragment = {
        val a = containers.map(ArrayRecord(asset_id = assetId) ++ _)
        sql"""|INSERT INTO
              |  loot_asset_containers(asset_id, world, x, y, z, block_id, facing, waterlogged, chest_type)
              |  ${Fragments.values(a)}
              |""".stripMargin
      }

      def makeContainersDeleteQuery(assetIds: NonEmptyList[Int]): Fragment = {
        sql"DELETE FROM loot_asset_containers WHERE ${Fragments.in(fr"asset_id", assetIds)}"
      }

      def makeItemsInsertQuery(assetId: Int, items: NonEmptyList[LootAssetItemRecordRepr]): Fragment = {
        val a = items.map(ArrayRecord(asset_id = assetId) ++ _)
        sql"INSERT INTO loot_asset_items(asset_id, slot, item, quantity) ${Fragments.values(a)}"
      }

      def makeItemsDeleteQuery(assetIds: NonEmptyList[Int]): Fragment = {
        sql"DELETE FROM loot_asset_items WHERE ${Fragments.in(fr"asset_id", assetIds)}"
      }

      override def initialize(): F[Unit] = ASSET_CREATE_QUERIES.traverse_(_.update.run).transact(transactor)

      override def getAllLootAssets(): F[List[LootAsset]] = for {
        queryResult <- ASSET_SELECT_QUERY
          .query[(LootAssetRecordRepr, Option[LootAssetContainerRecordRepr], Option[LootAssetItemRecordRepr])]
          .to[List]
          .transact(transactor)
        lootAsset   <- queryResult.groupMap(_._1)(v => (v._2, v._3)).toList.traverse {
          case (repr, data) => {
            val (containers, items) = data.separate
            reprToLootAsset(repr, containers.flatten.distinct, items.flatten)
          }
        }
      } yield lootAsset

      override def upsertLootAssets(lootAssets: List[LootAsset]): F[Unit] = {
        lootAssets.toNel.traverse_(_.traverse_(createUpsertLootAssetTransaction)).transact(transactor)
      }

      override def deleteLootAssets(ids: NonEmptyList[Int]): F[Unit] = {
        val program = for {
          _ <- makeItemsDeleteQuery(ids).update.run
          _ <- makeContainersDeleteQuery(ids).update.run
          _ <- makeAssetsDeleteQuery(ids).update.run
        } yield ()
        program.transact(transactor)
      }

      private def createUpsertLootAssetTransaction(lootAsset: LootAsset): ConnectionIO[Unit] = {
        val (recordReprAsset, recordReprContainers, recordReprItems) = lootAssetToRepr(lootAsset)
        for {
          _  <- lootAsset.id.traverse(id => makeContainersDeleteQuery(NonEmptyList.one(id)).update.run)
          _  <- lootAsset.id.traverse(id => makeItemsDeleteQuery(NonEmptyList.one(id)).update.run)
          id <- makeAssetUpsertQuery(recordReprAsset).update.withUniqueGeneratedKeys[Int]("id")
          _  <- recordReprContainers.toNel.traverse(makeContainersInsertQuery(id, _).update.run)
          _  <- recordReprItems.toNel.traverse(makeItemsInsertQuery(id, _).update.run)
        } yield ()
      }
    }
  }
}
