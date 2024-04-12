package com.github.chencmd.lootcontainerutil.minecraft.bukkit

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTNel
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.effect.SyncIO
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import dev.array21.bukkitreflectionlib.ReflectionUtil
import java.lang.reflect.Constructor

type NMSNBTTag

object NMSNBTTag {
  lazy val _clazz = ReflectionUtil.getMinecraftClass("nbt.Tag")

  def convert[A](a: A)(using NNT: NMSNBTTagConverter[A]): SyncIO[NNT.NMSTagType] = NNT.valueOf(a)

  trait NMSNBTTagConverter[A] {
    type NMSTagType

    def valueOf(a: A): SyncIO[NMSTagType]
  }

  object NMSNBTTagConverter {

    import NBTTag.*

    def apply[A](using NNT: NMSNBTTagConverter[A]): NNT.type = NNT

    given NMSNBTTagConverter[NBTTag] with {
      override type NMSTagType = NMSNBTTag

      override def valueOf(a: NBTTag): SyncIO[NMSNBTTag] = {
        a match {
          case b: NBTTagByte      => NMSNBTTagConverter[NBTTagByte].valueOf(b)
          case b: NBTTagShort     => NMSNBTTagConverter[NBTTagShort].valueOf(b)
          case b: NBTTagInt       => NMSNBTTagConverter[NBTTagInt].valueOf(b)
          case b: NBTTagLong      => NMSNBTTagConverter[NBTTagLong].valueOf(b)
          case b: NBTTagFloat     => NMSNBTTagConverter[NBTTagFloat].valueOf(b)
          case b: NBTTagDouble    => NMSNBTTagConverter[NBTTagDouble].valueOf(b)
          case b: NBTTagString    => NMSNBTTagConverter[NBTTagString].valueOf(b)
          case b: NBTTagByteArray => NMSNBTTagConverter[NBTTagByteArray].valueOf(b)
          case b: NBTTagIntArray  => NMSNBTTagConverter[NBTTagIntArray].valueOf(b)
          case b: NBTTagLongArray => NMSNBTTagConverter[NBTTagLongArray].valueOf(b)
          case b: NBTTagList      => NMSNBTTagConverter[NBTTagList].valueOf(b)
          case b: NBTTagCompound  => NMSNBTTagConverter[NBTTagCompound].valueOf(b)
        }
      }
    }

    given NMSNBTTagConverter[NBTTagCompound] with {
      type NMSNBTTagConvertedCompound = NMSNBTTagCompound
      override type NMSTagType        = NMSNBTTagConvertedCompound

      override def valueOf(a: NBTTagCompound): SyncIO[NMSNBTTagConvertedCompound] = for {
        a <- a.value.toList.traverse { case (k, v) => NMSNBTTag.convert(v).map(k -> _) }
        x <- NMSNBTTagCompound(a.toMap)
      } yield x
    }

    given NMSNBTTagConverter[NBTTagByte] with {
      type NMSNBTTagConvertedByte <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedByte

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.ByteTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagByte): SyncIO[NMSNBTTagConvertedByte] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedByte]
      }
    }

    given NMSNBTTagConverter[NBTTagShort] with {
      type NMSNBTTagConvertedShort <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedShort

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.ShortTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagShort): SyncIO[NMSNBTTagConvertedShort] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedShort]
      }
    }

    given NMSNBTTagConverter[NBTTagInt] with {
      type NMSNBTTagConvertedInt <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedInt

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.IntTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagInt): SyncIO[NMSNBTTagConvertedInt] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedInt]
      }
    }

    given NMSNBTTagConverter[NBTTagLong] with {
      type NMSNBTTagConvertedLong <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedLong

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.LongTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagLong): SyncIO[NMSNBTTagConvertedLong] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedLong]
      }
    }

    given NMSNBTTagConverter[NBTTagFloat] with {
      type NMSNBTTagConvertedFloat <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedFloat

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.FloatTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagFloat): SyncIO[NMSNBTTagConvertedFloat] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedFloat]
      }
    }

    given NMSNBTTagConverter[NBTTagDouble] with {
      type NMSNBTTagConvertedDouble <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedDouble

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.DoubleTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagDouble): SyncIO[NMSNBTTagConvertedDouble] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedDouble]
      }
    }

    given NMSNBTTagConverter[NBTTagString] with {
      type NMSNBTTagConvertedString <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedString

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.StringTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagString): SyncIO[NMSNBTTagConvertedString] = SyncIO {
        valueOfMethod.invoke(null, a.value).asInstanceOf[NMSNBTTagConvertedString]
      }
    }

    given NMSNBTTagConverter[NBTTagByteArray] with {
      type NMSNBTTagConvertedByteArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedByteArray

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.ByteArrayTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagByteArray): SyncIO[NMSNBTTagConvertedByteArray] = SyncIO {
        valueOfMethod.invoke(null, a.value.asJava).asInstanceOf[NMSNBTTagConvertedByteArray]
      }
    }

    given NMSNBTTagConverter[NBTTagIntArray] with {
      type NMSNBTTagConvertedIntArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedIntArray

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.IntArrayTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagIntArray): SyncIO[NMSNBTTagConvertedIntArray] = SyncIO {
        valueOfMethod.invoke(null, a.value.asJava).asInstanceOf[NMSNBTTagConvertedIntArray]
      }
    }

    given NMSNBTTagConverter[NBTTagLongArray] with {
      type NMSNBTTagConvertedLongArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedLongArray

      private lazy val clazz         = ReflectionUtil.getMinecraftClass("nbt.LongArrayTag")
      private lazy val valueOfMethod = ReflectionUtil.getMethod(clazz, "valueOf")

      override def valueOf(a: NBTTagLongArray): SyncIO[NMSNBTTagConvertedLongArray] = SyncIO {
        valueOfMethod.invoke(null, a.value.asJava).asInstanceOf[NMSNBTTagConvertedLongArray]
      }
    }

    given NMSNBTTagConverter[NBTTagList] with {
      type NMSNBTTagConvertedList <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedList

      private lazy val clazz       = ReflectionUtil.getMinecraftClass("nbt.ListTag")
      private lazy val constructor = ReflectionUtil
        .getConstructor(clazz, classOf[java.util.List[?]], classOf[Byte])
        .asInstanceOf[Constructor[NMSNBTTagConvertedList]]

      override def valueOf(a: NBTTagList): SyncIO[NMSNBTTagConvertedList] = for {
        convertedList <- a.toList.traverse(NMSNBTTag.convert)
        typeId = a.value match {
          case None                      => 0
          case Some(NBTNel.Byte(_))      => 1
          case Some(NBTNel.Short(_))     => 2
          case Some(NBTNel.Int(_))       => 3
          case Some(NBTNel.Long(_))      => 4
          case Some(NBTNel.Float(_))     => 5
          case Some(NBTNel.Double(_))    => 6
          case Some(NBTNel.ByteArray(_)) => 7
          case Some(NBTNel.String(_))    => 8
          case Some(NBTNel.List(_))      => 9
          case Some(NBTNel.Compound(_))  => 10
          case Some(NBTNel.IntArray(_))  => 11
          case Some(NBTNel.LongArray(_)) => 12
        }

        list <- SyncIO(constructor.newInstance(convertedList.asJava, typeId))
      } yield list
    }
  }
}
