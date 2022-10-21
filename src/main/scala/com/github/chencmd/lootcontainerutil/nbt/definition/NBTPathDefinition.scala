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

private sealed trait NBTTagListTrait[+A <: NBTTag](val value: List[A])

type NBTTagListType = NBTTag & NBTTagListTrait[NBTTag]

enum NBTTag derives CanEqual {
  case NBTTagCompound(value: Map[String, NBTTag])
  case NBTTagString(value: String)
  case NBTTagByte(value: Byte)
  case NBTTagShort(value: Short)
  case NBTTagInt(value: Int)
  case NBTTagLong(value: Long)
  case NBTTagFloat(value: Float)
  case NBTTagDouble(value: Double)
  case NBTTagCompoundList(override val value: List[NBTTagCompound])
      extends NBTTag
      with NBTTagListTrait[NBTTagCompound](value)
  case NBTTagStringList(override val value: List[NBTTagString])
      extends NBTTag
      with NBTTagListTrait[NBTTagString](value)
  case NBTTagByteList(override val value: List[NBTTagByte])
      extends NBTTag
      with NBTTagListTrait[NBTTagByte](value)
  case NBTTagShortList(override val value: List[NBTTagShort])
      extends NBTTag
      with NBTTagListTrait[NBTTagShort](value)
  case NBTTagIntList(override val value: List[NBTTagInt])
      extends NBTTag
      with NBTTagListTrait[NBTTagInt](value)
  case NBTTagLongList(override val value: List[NBTTagLong])
      extends NBTTag
      with NBTTagListTrait[NBTTagLong](value)
  case NBTTagFloatList(override val value: List[NBTTagFloat])
      extends NBTTag
      with NBTTagListTrait[NBTTagFloat](value)
  case NBTTagDoubleList(override val value: List[NBTTagDouble])
      extends NBTTag
      with NBTTagListTrait[NBTTagDouble](value)
  case NBTTagNestedList(override val value: List[NBTTagListType])
      extends NBTTag
      with NBTTagListTrait[NBTTagListType](value)
}