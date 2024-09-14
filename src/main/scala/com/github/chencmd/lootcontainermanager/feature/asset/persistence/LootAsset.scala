package com.github.chencmd.lootcontainermanager.feature.asset.persistence

import com.github.chencmd.lootcontainermanager.minecraft.bukkit.BlockLocation

import java.util.UUID

import org.bukkit.NamespacedKey
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

enum LootAsset(
  val id: Option[Int],
  val uuid: UUID,
  val typ: String,
  val name: Option[String],
  val containers: List[LootAssetContainer]
) {
  case Fixed(
    override val id: Option[Int],
    override val uuid: UUID,
    override val name: Option[String],
    override val containers: List[LootAssetContainer],
    items: List[LootAssetItem]
  ) extends LootAsset(id, uuid, "fixed", name, containers)

  case Random(
    override val id: Option[Int],
    override val uuid: UUID,
    override val name: Option[String],
    override val containers: List[LootAssetContainer],
    lootTable: NamespacedKey
  ) extends LootAsset(id, uuid, "random", name, containers)
}
