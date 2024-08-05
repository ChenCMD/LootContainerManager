package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAsset
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

package object terms {
  // 更新情報を id として持たない理由としては、未登録のアセットは id を持たないため
  case class LootAssetCache(
    assets: Map[BlockLocation, Map[BlockLocation, LootAsset]],
    updatedAssetLocations: Set[BlockLocation],
    deletedAssetIds: Set[Int]
  )

  object LootAssetCache {
    def empty: LootAssetCache = LootAssetCache(Map.empty, Set.empty, Set.empty)
  }
}
