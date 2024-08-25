package com.github.chencmd.lootcontainerutil.adapter.database

import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.terms.LootAssetCache

import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.implicits.*

object LootAssetRepositoryCache {
  def createInstr[F[_]: Sync](
    cacheRef: Ref[F, LootAssetCache]
  ): LootAssetPersistenceCacheInstr[F] = new LootAssetPersistenceCacheInstr[F] {
    override def askLootAssetLocationsNear(location: BlockLocation): F[List[LootAsset]] = {
      // get from 5x5 chunk
      val chunk = location.toChunkLocation
      cacheRef.get.map { cache =>
        for {
          x <- (-2 to 2).toList
          y <- (-2 to 2).toList
          z <- (-2 to 2).toList
          chunkLocation = BlockLocation(chunk.world, chunk.x + x, chunk.y + y, chunk.z + z)
          location <- cache.assets.get(chunkLocation).map(_.values.toList.distinct).orEmpty
          asset    <- cache.mapping.get(location).toList
        } yield asset
      }
    }

    override def askLootAssetLocationAt(location: BlockLocation): F[Option[LootAsset]] = {
      cacheRef.get.map(_.askLootAssetLocationAt(location))
    }

    override def askIfLootAssetPresentAt(location: BlockLocation): F[Boolean] = {
      askLootAssetLocationAt(location).map(_.nonEmpty)
    }

    override def updateLootAsset(asset: LootAsset): F[Unit] = for {
      _ <- cacheRef.update { s =>
        val uuid = asset.uuid
        val p    = asset.containers.map(_.location)
        LootAssetCache(
          p.foldLeft(s.assets)((assets, loc) =>
            assets.updatedWith(loc.toChunkLocation)(_.fold(Map(loc -> uuid))(_ + (loc -> uuid)).some)
          ),
          s.mapping + (uuid -> asset),
          s.updatedAssetLocations + uuid,
          s.deletedAssetIds
        )
      }
    } yield ()

    override def deleteLootAssetLocationAt(location: BlockLocation): F[Unit] = for {
      asset <- askLootAssetLocationAt(location)
      asset <- asset.fold(SystemException.raise("Asset not found"))(_.pure[F])
      _     <- Sync[F].delay { println(s"Deleting asset: $asset") }
      _     <- cacheRef.update { s =>
        // LootAssetCache(
        //   s.assets.updatedWith(location.toChunkLocation)(_.map(_ - location)),
        //   s.updatedAssetLocations - location,
        //   asset.id.fold(s.deletedAssetIds)(s.deletedAssetIds + _)
        // )
        val uuid = asset.uuid
        val p    = asset.containers.map(_.location)
        LootAssetCache(
          p.foldLeft(s.assets)((assets, loc) => assets.updatedWith(loc.toChunkLocation)(_.map(_ - loc))),
          s.mapping - uuid,
          s.updatedAssetLocations - uuid,
          asset.id.fold(s.deletedAssetIds)(s.deletedAssetIds + _)
        )
      }
      cache <- cacheRef.get
      _     <- Sync[F].delay { println(s"caches: ${cache}") }
    } yield ()
  }
}
