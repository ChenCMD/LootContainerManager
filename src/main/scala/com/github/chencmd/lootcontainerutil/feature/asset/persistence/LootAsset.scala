package com.github.chencmd.lootcontainerutil.feature.asset.persistence

import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation

import java.util.UUID
import org.bukkit.block.BlockFace
import org.bukkit.block.data.`type`.Chest

case class LootAssetContainer(
  location: BlockLocation,
  blockId: String,
  facing: Option[BlockFace],
  waterlogged: Option[Boolean],
  chestType: Option[Chest.Type]
)

case class LootAssetItem(
  slot: Int,
  item: String,
  quantity: Int
)

case class LootAsset(
  id: Option[Int],
  uuid: UUID,
  name: Option[String],
  containers: List[LootAssetContainer],
  items: List[LootAssetItem]
)
