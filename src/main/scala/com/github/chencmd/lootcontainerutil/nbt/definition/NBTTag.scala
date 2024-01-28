package com.github.chencmd.lootcontainerutil.nbt.definition

import cats.data.NonEmptyList
import cats.implicits.*

import scala.util.chaining.*

import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

trait NBTTagListOps(value: Option[NBTNel]) {
  def toList: List[NBTTag] = value.map(_.values.toList).orEmpty
}

enum NBTTag(val value: Any) {
  case NBTTagCompound(override val value: Map[String, NBTTag]) extends NBTTag(value)
  case NBTTagByte(override val value: Byte)                    extends NBTTag(value)
  case NBTTagShort(override val value: Short)                  extends NBTTag(value)
  case NBTTagInt(override val value: Int)                      extends NBTTag(value)
  case NBTTagLong(override val value: Long)                    extends NBTTag(value)
  case NBTTagFloat(override val value: Float)                  extends NBTTag(value)
  case NBTTagDouble(override val value: Double)                extends NBTTag(value)
  case NBTTagString(override val value: String)                extends NBTTag(value)
  case NBTTagByteArray(override val value: Vector[NBTTagByte]) extends NBTTag(value)
  case NBTTagIntArray(override val value: Vector[NBTTagInt])   extends NBTTag(value)
  case NBTTagLongArray(override val value: Vector[NBTTagLong]) extends NBTTag(value)
  case NBTTagList(override val value: Option[NBTNel])          extends NBTTag(value) with NBTTagListOps(value)

  def toSNBT: String = this match {
    case NBTTagCompound(value)  => value
        .map { case (k, v) => s"${quote(k)}:${v.toSNBT}" }
        .mkString("{", ",", "}")
    case NBTTagByte(value)      => s"${value}b"
    case NBTTagShort(value)     => s"${value}s"
    case NBTTagInt(value)       => value.toString
    case NBTTagLong(value)      => s"${value}L"
    case NBTTagFloat(value)     => decimalFormat(value, "f")
    case NBTTagDouble(value)    => decimalFormat(value, "d")
    case NBTTagString(value)    => quote(value)
    case NBTTagByteArray(value) => listToString(value, "B")
    case NBTTagIntArray(value)  => listToString(value, "I")
    case NBTTagLongArray(value) => listToString(value, "L")
    case NBTTagList(value)      => listToString(value.map(_.values.toList).orEmpty)
  }

  protected def quote(str: String): String = str
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .pipe(s => s"\"$s\"")

  protected def decimalFormat(number: Number, suffix: String): String                = {
    DecimalFormat("0", DecimalFormatSymbols.getInstance(Locale.ENGLISH))
      .tap(_.setMaximumFractionDigits(340))
      .pipe(_.format(number) + suffix)
  }
  protected def listToString[A <: NBTTag](list: Seq[A], prefix: String = ""): String = {
    list
      .map(_.toSNBT)
      .mkString("[" + Option(prefix).filter(_.nonEmpty).map(_ + ";").orEmpty, ",", "]")
  }
}

object NBTTag {
  def listFrom(): NBTTagList             = NBTTagList(None)
  def listFrom(list: NBTNel): NBTTagList = NBTTagList(Some(list))
}

sealed trait NBTNel {
  type Elems <: NBTTag
  val values: NonEmptyList[Elems]
}
object NBTNel       {
  import NBTTag.*
  case class Compound(val values: NonEmptyList[NBTTagCompound]) extends NBTNel {
    type Elems = NBTTagCompound
  }
  case class Byte(val values: NonEmptyList[NBTTagByte])         extends NBTNel {
    type Elems = NBTTagByte
  }
  case class Short(val values: NonEmptyList[NBTTagShort])       extends NBTNel {
    type Elems = NBTTagShort
  }
  case class Int(val values: NonEmptyList[NBTTagInt])           extends NBTNel {
    type Elems = NBTTagInt
  }
  case class Long(val values: NonEmptyList[NBTTagLong])         extends NBTNel {
    type Elems = NBTTagLong
  }
  case class Float(val values: NonEmptyList[NBTTagFloat])       extends NBTNel {
    type Elems = NBTTagFloat
  }
  case class Double(val values: NonEmptyList[NBTTagDouble])     extends NBTNel {
    type Elems = NBTTagDouble
  }
  case class String(val values: NonEmptyList[NBTTagString])     extends NBTNel {
    type Elems = NBTTagString
  }
  case class ByteArray(val values: NonEmptyList[NBTTagByte])    extends NBTNel {
    type Elems = NBTTagByte
  }
  case class IntArray(val values: NonEmptyList[NBTTagInt])      extends NBTNel {
    type Elems = NBTTagInt
  }
  case class LongArray(val values: NonEmptyList[NBTTagLong])    extends NBTNel {
    type Elems = NBTTagLong
  }
  case class List(val values: NonEmptyList[NBTTagList | NBTTagByteArray | NBTTagIntArray | NBTTagLongArray])
      extends NBTNel {
    type Elems = NBTTagList | NBTTagByteArray | NBTTagIntArray | NBTTagLongArray
  }
}
