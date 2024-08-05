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
          chunkLocation = BlockLocation(chunk.w, chunk.x + x, chunk.y + y, chunk.z + z)
          locations <- cache.assets.get(chunkLocation).map(_.values.toList).orEmpty
        } yield locations
      }
    }

    override def askLootAssetLocationAt(location: BlockLocation): F[Option[LootAsset]] = {
      cacheRef.get.map(_.assets.get(location.toChunkLocation).flatMap(_.get(location)))
    }

    override def askIfLootAssetPresentAt(location: BlockLocation): F[Boolean] = {
      askLootAssetLocationAt(location).map(_.nonEmpty)
    }

    override def updateLootAsset(asset: LootAsset): F[Unit] = for {
      _ <- cacheRef.update { s =>
        val p = asset.location -> asset
        LootAssetCache(
          s.assets.updatedWith(asset.location.toChunkLocation)(_.fold(Map(p))(_ + p).some),
          s.updatedAssetLocations + asset.location,
          s.deletedAssetIds
        )
      }
    } yield ()

    override def deleteLootAssetLocationAt(location: BlockLocation): F[Unit] = for {
      asset <- askLootAssetLocationAt(location)
      asset <- asset.fold(SystemException.raise("Asset not found"))(_.pure[F])
      _     <- Sync[F].delay { println(s"Deleting asset: $asset") }
      _     <- cacheRef.update { s =>
        LootAssetCache(
          s.assets.updatedWith(location.toChunkLocation)(_.map(_ - location)),
          s.updatedAssetLocations - location,
          asset.id.fold(s.deletedAssetIds)(s.deletedAssetIds + _)
        )
      }
      cache <- cacheRef.get
      _     <- Sync[F].delay { println(s"caches: ${cache}") }
    } yield ()
  }
}
