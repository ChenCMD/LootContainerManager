package com.github.chencmd.lootcontainermanager.adapter.database

import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetContainer
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetItem
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainermanager.generic.extensions.CastExt.*
import com.github.chencmd.lootcontainermanager.generic.extra.FragmentsExtra
import com.github.chencmd.lootcontainermanager.generic.instances.MetaInstances.given
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.implicits.*

import java.util.UUID

import org.bukkit.NamespacedKey
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

import com.github.tarao.record4s.ArrayRecord
import doobie.*
import doobie.implicits.*

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
        ("typ", String),
        ("name", Option[String]),
        ("lootTable", Option[String])
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
      val assetRepr  = ArrayRecord(
        id = lootAsset.id,
        uuid = lootAsset.uuid,
        typ = lootAsset.typ,
        name = lootAsset.name,
        lootTable = lootAsset.downcastOrNone[LootAsset.Random].map(_.lootTable.toString)
      )
      val containers = lootAsset.containers.map(lootAssetContainerToRepr)
      val items      = lootAsset.downcastOrNone[LootAsset.Fixed].map(_.items.map(lootAssetItemToRepr)).orEmpty
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
      asset <- recordRepr.typ match {
        case "fixed"  => LootAsset.Fixed(recordRepr.id, recordRepr.uuid, recordRepr.name, containers, items).pure[F]
        case "random" => for {
            ltKey <- recordRepr.lootTable
              .map(NamespacedKey.fromString)
              .fold(SystemException.raise(s"Invalid loot table key: ${recordRepr.lootTable}"))(_.pure[F])
            asset = LootAsset.Random(recordRepr.id, recordRepr.uuid, recordRepr.name, containers, ltKey)
          } yield asset
        case _        => SystemException.raise(s"Invalid asset type: ${recordRepr.typ}")
      }
    } yield asset

    new LootAssetPersistenceInstr[F] {
      val ASSET_SELECT_QUERY = sql"""|
        |SELECT
        |   asset.id, asset.uuid, asset.typ, asset.name, asset.loot_table,
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
        sql"INSERT OR REPLACE INTO loot_assets(id, uuid, typ, name, loot_table) VALUES ${FragmentsExtra.tupled(assetRepr.values)}"
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
