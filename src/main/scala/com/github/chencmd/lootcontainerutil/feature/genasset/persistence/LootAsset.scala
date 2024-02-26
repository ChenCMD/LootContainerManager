package com.github.chencmd.lootcontainerutil.feature.genasset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.Location

case class LootAssetItem(
  slot: Int,
  item: String,
  quantity: Int
)

case class LootAsset(
  location: Location[Int],
  blockId: String,
  facing: Option[String],
  waterlogged: Option[Boolean],
  chestType: Option[String],
  items: List[LootAssetItem]
)
