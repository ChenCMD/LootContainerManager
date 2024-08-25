package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import java.util.UUID

package object terms {
  case class LootAssetCache(
    assets: Map[BlockLocation, Map[BlockLocation, UUID]],
    mapping: Map[UUID, LootAsset],
    updatedAssetLocations: Set[UUID],
    deletedAssetIds: Set[Int]
  ) {
    def askLootAssetLocationAt(location: BlockLocation): Option[LootAsset] = for {
      forChunk <- assets.get(location.toChunkLocation)
      uuid     <- forChunk.get(location)
      asset    <- mapping.get(uuid)
    } yield asset
  }

  object LootAssetCache {
    def empty: LootAssetCache = LootAssetCache(Map.empty, Map.empty, Set.empty, Set.empty)
  }
}
