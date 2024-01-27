package com.github.chencmd.lootcontainerutil.nbt.definition

import NBTTag.*
import cats.data.StateT
import cats.arrow.FunctionK
import cats.implicits.*
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone

import scala.util.chaining.*

case class NBTPath(root: NBTPathRootNode, nodes: List[NBTPathNode]) {
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
                .downcastOrNone[NBTTagList]
                .fold(List.empty)(_.toList)
          case NBTPathNode.MatchElement(pattern) =>
            tag
              .downcastOrNone[NBTTagList]
              .fold(List.empty)(_.toList)
              .filter {
                case compound: NBTTagCompound => isMatch(compound, pattern)
                case _                        => false
              }
              .toList
          case NBTPathNode.IndexedElement(index) =>
            tag
              .downcastOrNone[NBTTagList]
              .flatMap(_.toList.pipe { list =>
                list.lift(Math.floorMod(index, list.length))
              })
              .toList
          case NBTPathNode.CompoundChild(name) =>
            getChild(tag, name).toList
        }
      }

    val optionToList =
      FunctionK.lift[Option, Seq]([A] => (_: Option[A]).toList)
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
      case (tList: NBTTagList, eList: NBTTagList) => {
        val eElems = eList.toList
        val tElems = tList.toList
        eElems.forall { eElement =>
          tElems.exists { tElement =>
            isMatch(tElement, eElement)
          }
        }
      }
      case (tList: NBTTagByteArray, eList: NBTTagByteArray) => tList.value.sameElements(eList.value)
      case (tList: NBTTagIntArray, eList: NBTTagIntArray) => tList.value.sameElements(eList.value)
      case (tList: NBTTagLongArray, eList: NBTTagLongArray) => tList.value.sameElements(eList.value)
      case _ => target.value == expect.value
    }
  }
}

enum NBTPathRootNode {
  case MatchRootObject(pattern: NBTTagCompound)
  case MatchObject(name: String, pattern: NBTTagCompound)
  case CompoundChild(name: String)
}

enum NBTPathNode {
  case MatchObject(name: String, pattern: NBTTagCompound)
  case AllElements()
  case MatchElement(pattern: NBTTagCompound)
  case IndexedElement(index: Int)
  case CompoundChild(name: String)
}
