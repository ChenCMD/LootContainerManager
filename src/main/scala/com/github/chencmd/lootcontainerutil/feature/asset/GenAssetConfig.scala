package com.github.chencmd.lootcontainermanager.feature.asset

import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPath
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPathInterpolation

import scala.util.matching.Regex

type ItemMapper = (NBTPath, NBTPathInterpolation)

enum DataSource(val path: NBTPath) {
  case Block(val world: String, val x: Int, val y: Int, val z: Int, override val path: NBTPath) extends DataSource(path)
  case Storage(val namespace: String, override val path: NBTPath)                               extends DataSource(path)
  case Entity(val selector: String, override val path: NBTPath)                                 extends DataSource(path)
}

enum ItemGenerator(
  val predicate: Regex,
  val id: String,
  val preCommand: List[String]
) {
  case WithLootTable(
    override val predicate: Regex,
    override val id: String,
    override val preCommand: List[String]
  ) extends ItemGenerator(predicate, id, preCommand)

  case WithMCFunction(
    override val predicate: Regex,
    override val id: String,
    override val preCommand: List[String],
    val functionOutput: DataSource
  ) extends ItemGenerator(predicate, id, preCommand)

  case WithItemId(
    override val predicate: Regex,
    override val id: String,
    override val preCommand: List[String]
  ) extends ItemGenerator(predicate, id, preCommand)
}

final case class GenAssetConfig(
  toItemIdentifier: List[ItemMapper],
  toItem: List[ItemGenerator]
)
