package com.github.chencmd.lootcontainerutil.feature.genasset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.Location

import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

case class LootAssetItem(
  slot: Int,
  item: String,
  quantity: Int
)

case class LootAsset(
  location: Location[Int],
  blockId: String,
  name: Option[String],
  facing: Option[BlockFace],
  waterlogged: Option[Boolean],
  chestType: Option[Chest.Type],
  items: List[LootAssetItem]
)
