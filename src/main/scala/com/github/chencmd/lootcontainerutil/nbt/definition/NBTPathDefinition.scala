package com.github.chencmd.lootcontainerutil.nbt.definition

case class NBTPath(root: NBTPathRootNode, nodes: List[NBTPathNode]) derives CanEqual

import NBTTag.*

enum NBTPathRootNode derives CanEqual {
  case MatchRootObject(pattern: NBTTagCompound)
  case MatchObject(name: String, pattern: NBTTagCompound)
  case CompoundChild(name: String)
}

enum NBTPathNode derives CanEqual {
  case MatchObject(name: String, pattern: NBTTagCompound)
  case AllElements()
  case MatchElement(pattern: NBTTagCompound)
  case IndexedElement(index: Int)
  case CompoundChild(name: String)
}

private sealed trait NBTTagListTrait

type NBTTagListType = NBTTag & NBTTagListTrait

enum NBTTag derives CanEqual {
  case NBTTagCompound(value: Map[String, NBTTag])
  case NBTTagString(value: String)
  case NBTTagByte(value: Byte)
  case NBTTagShort(value: Short)
  case NBTTagInt(value: Int)
  case NBTTagLong(value: Long)
  case NBTTagFloat(value: Float)
  case NBTTagDouble(value: Double)
  case NBTTagCompoundList(value: List[NBTTagCompound]) extends NBTTag with NBTTagListTrait
  case NBTTagStringList(value: List[String]) extends NBTTag with NBTTagListTrait
  case NBTTagByteList(value: List[Byte]) extends NBTTag with NBTTagListTrait
  case NBTTagShortList(value: List[Short]) extends NBTTag with NBTTagListTrait
  case NBTTagIntList(value: List[Int]) extends NBTTag with NBTTagListTrait
  case NBTTagLongList(value: List[Long]) extends NBTTag with NBTTagListTrait
  case NBTTagFloatList(value: List[Float]) extends NBTTag with NBTTagListTrait
  case NBTTagDoubleList(value: List[Double]) extends NBTTag with NBTTagListTrait
  case NBTTagNestedList(value: List[NBTTagListTrait]) extends NBTTag with NBTTagListTrait
}