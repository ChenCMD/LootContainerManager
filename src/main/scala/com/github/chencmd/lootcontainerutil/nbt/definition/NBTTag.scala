package com.github.chencmd.lootcontainerutil.nbt.definition

import cats.data.NonEmptyList
import cats.implicits.*

import scala.util.chaining.*

import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale
import cats.kernel.Monoid
import cats.kernel.Semigroup

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

  given Monoid[NBTTag.NBTTagByte] with      {
    def empty = NBTTagByte(Monoid[Byte].empty)

    def combine(x: NBTTagByte, y: NBTTagByte): NBTTagByte = NBTTagByte(Semigroup[Byte].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagShort] with     {
    def empty = NBTTagShort(Monoid[Short].empty)

    def combine(x: NBTTagShort, y: NBTTagShort): NBTTagShort = NBTTagShort(Semigroup[Short].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagInt] with       {
    def empty = NBTTagInt(Monoid[Int].empty)

    def combine(x: NBTTagInt, y: NBTTagInt): NBTTagInt = NBTTagInt(Semigroup[Int].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagLong] with      {
    def empty = NBTTagLong(Monoid[Long].empty)

    def combine(x: NBTTagLong, y: NBTTagLong): NBTTagLong = NBTTagLong(Semigroup[Long].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagFloat] with     {
    def empty = NBTTagFloat(Monoid[Float].empty)

    def combine(x: NBTTagFloat, y: NBTTagFloat): NBTTagFloat = NBTTagFloat(Semigroup[Float].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagDouble] with    {
    def empty = NBTTagDouble(Monoid[Double].empty)

    def combine(x: NBTTagDouble, y: NBTTagDouble): NBTTagDouble =
      NBTTagDouble(Semigroup[Double].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagString] with    {
    def empty = NBTTagString(Monoid[String].empty)

    def combine(x: NBTTagString, y: NBTTagString): NBTTagString =
      NBTTagString(Semigroup[String].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagByteArray] with {
    def empty = NBTTagByteArray(Monoid[Vector[NBTTagByte]].empty)

    def combine(x: NBTTagByteArray, y: NBTTagByteArray): NBTTagByteArray =
      NBTTagByteArray(Semigroup[Vector[NBTTagByte]].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagIntArray] with  {
    def empty = NBTTagIntArray(Monoid[Vector[NBTTagInt]].empty)

    def combine(x: NBTTagIntArray, y: NBTTagIntArray): NBTTagIntArray =
      NBTTagIntArray(Semigroup[Vector[NBTTagInt]].combine(x.value, y.value))
  }
  given Monoid[NBTTag.NBTTagLongArray] with {
    def empty = NBTTagLongArray(Monoid[Vector[NBTTagLong]].empty)

    def combine(x: NBTTagLongArray, y: NBTTagLongArray): NBTTagLongArray =
      NBTTagLongArray(Semigroup[Vector[NBTTagLong]].combine(x.value, y.value))
  }
}

sealed trait NBTNel {
  type Elems <: NBTTag
  val values: NonEmptyList[Elems]
}
object NBTNel       {
  import NBTTag.*
  type ListTypes = NBTTagList | NBTTagByteArray | NBTTagIntArray | NBTTagLongArray
  case class Compound(val values: NonEmptyList[NBTTagCompound]) extends NBTNel { type Elems = NBTTagCompound }
  case class Byte(val values: NonEmptyList[NBTTagByte])         extends NBTNel { type Elems = NBTTagByte     }
  case class Short(val values: NonEmptyList[NBTTagShort])       extends NBTNel { type Elems = NBTTagShort    }
  case class Int(val values: NonEmptyList[NBTTagInt])           extends NBTNel { type Elems = NBTTagInt      }
  case class Long(val values: NonEmptyList[NBTTagLong])         extends NBTNel { type Elems = NBTTagLong     }
  case class Float(val values: NonEmptyList[NBTTagFloat])       extends NBTNel { type Elems = NBTTagFloat    }
  case class Double(val values: NonEmptyList[NBTTagDouble])     extends NBTNel { type Elems = NBTTagDouble   }
  case class String(val values: NonEmptyList[NBTTagString])     extends NBTNel { type Elems = NBTTagString   }
  case class ByteArray(val values: NonEmptyList[NBTTagByte])    extends NBTNel { type Elems = NBTTagByte     }
  case class IntArray(val values: NonEmptyList[NBTTagInt])      extends NBTNel { type Elems = NBTTagInt      }
  case class LongArray(val values: NonEmptyList[NBTTagLong])    extends NBTNel { type Elems = NBTTagLong     }
  case class List(val values: NonEmptyList[ListTypes])          extends NBTNel { type Elems = ListTypes      }

  given Semigroup[Compound] with  {
    def combine(x: Compound, y: Compound): Compound = Compound(x.values ::: y.values)
  }
  given Semigroup[Byte] with      {
    def combine(x: Byte, y: Byte): Byte = Byte(x.values ::: y.values)
  }
  given Semigroup[Short] with     {
    def combine(x: Short, y: Short): Short = Short(x.values ::: y.values)
  }
  given Semigroup[Int] with       {
    def combine(x: Int, y: Int): Int = Int(x.values ::: y.values)
  }
  given Semigroup[Long] with      {
    def combine(x: Long, y: Long): Long = Long(x.values ::: y.values)
  }
  given Semigroup[Float] with     {
    def combine(x: Float, y: Float): Float = Float(x.values ::: y.values)
  }
  given Semigroup[Double] with    {
    def combine(x: Double, y: Double): Double = Double(x.values ::: y.values)
  }
  given Semigroup[String] with    {
    def combine(x: String, y: String): String = String(x.values ::: y.values)
  }
  given Semigroup[ByteArray] with {
    def combine(x: ByteArray, y: ByteArray): ByteArray = ByteArray(x.values ::: y.values)
  }
  given Semigroup[IntArray] with  {
    def combine(x: IntArray, y: IntArray): IntArray = IntArray(x.values ::: y.values)
  }
  given Semigroup[LongArray] with {
    def combine(x: LongArray, y: LongArray): LongArray = LongArray(x.values ::: y.values)
  }
  given Semigroup[List] with      {
    def combine(x: List, y: List): List = List(x.values ::: y.values)
  }
}
