package com.github.chencmd.lootcontainerutil.nbt.definition

import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import scala.util.chaining.*

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

  override def toString: String = {
    import NBTTag.*

    def quote(str: String): String = str
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .pipe(s => s"\"$s\"")
    def listToString[A <: NBTTag](
        list: List[A],
        prefix: String = ""): String = {
      val semicolonAddedPrefix = if prefix.nonEmpty then s"$prefix;" else ""
      list
        .map(_.toString)
        .mkString(s"[$semicolonAddedPrefix", ",", "]")
    }

    def decimalFormat(number: Number, suffix: String): String = {
      DecimalFormat("0", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
        .tap(_.setMaximumFractionDigits(340))
        .pipe(_.format(number) + suffix)
    }

    this match {
      case NBTTagCompound(value) =>
        value
          .map { case (k, v) => s"${quote(k)}:${v.toString}" }
          .mkString("{", ",", "}")
      case NBTTagString(value)       => quote(value)
      case NBTTagByte(value)         => s"${value}b"
      case NBTTagShort(value)        => s"${value}s"
      case NBTTagInt(value)          => value.toString
      case NBTTagLong(value)         => s"${value}L"
      case NBTTagFloat(value)        => decimalFormat(value, "f")
      case NBTTagDouble(value)       => decimalFormat(value, "d")
      case NBTTagCompoundList(value) => listToString(value)
      case NBTTagStringList(value)   => listToString(value)
      case NBTTagByteList(value)     => listToString(value, "B")
      case NBTTagShortList(value)    => listToString(value)
      case NBTTagIntList(value)      => listToString(value, "I")
      case NBTTagLongList(value)     => listToString(value, "L")
      case NBTTagFloatList(value)    => listToString(value)
      case NBTTagDoubleList(value)   => listToString(value)
      case NBTTagNestedList(value)   => listToString(value)
    }
  }
}
