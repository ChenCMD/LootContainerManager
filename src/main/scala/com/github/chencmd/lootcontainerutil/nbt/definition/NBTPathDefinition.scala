package com.github.chencmd.lootcontainerutil.nbt.definition

import NBTTag.{NBTTagCompound, *}
import cats.data.StateT
import cats.arrow.FunctionK
import cats.implicits.given
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone

import scala.util.chaining.*

case class NBTPath(root: NBTPathRootNode, nodes: List[NBTPathNode])
    derives CanEqual {
  def isAccessible(tag: NBTTagCompound): Boolean = {
    access(tag).nonEmpty
  }
  
  def access(tag: NBTTagCompound): List[NBTTag] = {
    def advanceFromRoot(node: NBTPathRootNode) =
      StateT.modifyF[Option, NBTTag] { tag =>
        import NBTPathRootNode.*
        node match {
          case MatchRootObject(pattern) =>
            Option.when(isMatch(tag, pattern))(tag)
          case MatchObject(name, pattern) =>
            getChild(tag, name).flatMap { child =>
              Option.when(isMatch(child, pattern))(child)
            }
          case CompoundChild(name) =>
            getChild(tag, name)
        }
      }
    def advanceIntoChildrenNondet(node: NBTPathNode) =
      StateT.modifyF[List, NBTTag] { tag =>
        import NBTPathNode.*
        node match {
          case NBTPathNode.MatchObject(name, pattern) =>
            getChild(tag, name).flatMap { child =>
              Option.when(isMatch(child, pattern))(child)
            }.toList
          case NBTPathNode.AllElements() =>
            tag
              .downcastOrNone[NBTTagListType]
              .fold(List.empty)(_.value)
          case NBTPathNode.MatchElement(pattern) =>
            tag
              .downcastOrNone[NBTTagCompoundList]
              .fold(List.empty)(_.value)
              .filter(isMatch(_, pattern))
          case NBTPathNode.IndexedElement(index) =>
            tag
              .downcastOrNone[NBTTagListType]
              .flatMap(_.value.pipe { list =>
                list.lift(Math.floorMod(index, list.length))
              })
              .toList
          case NBTPathNode.CompoundChild(name) =>
            getChild(tag, name).toList
        }
      }

    val optionToList =
      FunctionK.lift[Option, List]([A] => (_: Option[A]).toList)
    val state =
      advanceFromRoot(root)
        .mapK(optionToList) >> nodes
        .traverse(advanceIntoChildrenNondet)
        .void

    state.runS(tag)
  }

  private def getChild(tag: NBTTag, key: String): Option[NBTTag] = for {
    compound <- tag.downcastOrNone[NBTTagCompound]: Option[NBTTagCompound]
    child <- compound.value.get(key)
  } yield child

  private def isMatch(target: NBTTag, expect: NBTTag): Boolean = {
    (target, expect) match {
      case (NBTTagCompound(tMap), NBTTagCompound(eMap)) =>
        eMap.forall { case (key, eValue) =>
          tMap.get(key).exists { tValue =>
            isMatch(tValue, eValue)
          }
        }
      case (tList: NBTTagListType, eList: NBTTagListType) =>
        eList.value.forall { eElement =>
          tList.value.exists { tElement =>
            isMatch(tElement, eElement)
          }
        }
      case _ => target == expect
    }
  }
}

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
