package com.github.chencmd.lootcontainerutil.feature.genasset

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTPathInterpolation
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTPath
import scala.util.matching.Regex

type ItemMapper = (NBTPath, NBTPathInterpolation)

enum DataSource {
  case Block(val world: String, val x: Double, val y: Double, val z: Double, val path: NBTPath)
  case Storage(val namespace: String, val path: NBTPath)
  case Entity(val selector: String, val path: NBTPath)
}

enum ItemGenerator(
  val predicate: Regex,
  val id: String,
  val preCommand: List[String]
) {
  case WithLootTable(override val predicate: Regex, override val id: String, override val preCommand: List[String])
      extends ItemGenerator(predicate, id, preCommand)

  case WithMCFunction(
    override val predicate: Regex,
    override val id: String,
    override val preCommand: List[String],
    val functionOutput: DataSource
  ) extends ItemGenerator(predicate, id, preCommand)
}

final case class GenAssetConfig(
  toItemIdentifier: List[ItemMapper],
  toItem: List[ItemGenerator]
)
