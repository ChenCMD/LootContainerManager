package com.github.chencmd.lootcontainerutil.minecraft.nms

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTNel
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.effect.kernel.Sync
import cats.implicits.*

import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

import dev.array21.bukkitreflectionlib.ReflectionUtil

type NMSNBTTag

object NMSNBTTag {
  lazy val _clazz = ReflectionUtil.getMinecraftClass("nbt.Tag")
  def convert[F[_]: Sync, A](a: A)(using NNT: NMSNBTTagConverter[A]): F[NNT.NMSTagType] = NNT.valueOf(a)

  trait NMSNBTTagConverter[A] {
    type NMSTagType <: NMSNBTTag

    def valueOf[F[_]: Sync](a: A): F[NMSTagType]
  }

  object NMSNBTTagConverter {

    import NBTTag.*

    def apply[A](using NNT: NMSNBTTagConverter[A]): NNT.type = NNT

    given NMSNBTTagConverter[NBTTag] with {
      override type NMSTagType = NMSNBTTag

      override def valueOf[F[_]: Sync](a: NBTTag): F[NMSNBTTag] = {
        a match {
          case b: NBTTagByte      => NMSNBTTagConverter[NBTTagByte].valueOf(b).widen
          case b: NBTTagShort     => NMSNBTTagConverter[NBTTagShort].valueOf(b).widen
          case b: NBTTagInt       => NMSNBTTagConverter[NBTTagInt].valueOf(b).widen
          case b: NBTTagLong      => NMSNBTTagConverter[NBTTagLong].valueOf(b).widen
          case b: NBTTagFloat     => NMSNBTTagConverter[NBTTagFloat].valueOf(b).widen
          case b: NBTTagDouble    => NMSNBTTagConverter[NBTTagDouble].valueOf(b).widen
          case b: NBTTagString    => NMSNBTTagConverter[NBTTagString].valueOf(b).widen
          case b: NBTTagByteArray => NMSNBTTagConverter[NBTTagByteArray].valueOf(b).widen
          case b: NBTTagIntArray  => NMSNBTTagConverter[NBTTagIntArray].valueOf(b).widen
          case b: NBTTagLongArray => NMSNBTTagConverter[NBTTagLongArray].valueOf(b).widen
          case b: NBTTagList      => NMSNBTTagConverter[NBTTagList].valueOf(b).widen
          case b: NBTTagCompound  => NMSNBTTagConverter[NBTTagCompound].valueOf(b).widen
        }
      }
    }

    given NMSNBTTagConverter[NBTTagCompound] with {
      type NMSNBTTagConvertedCompound = NMSNBTTagCompound
      override type NMSTagType        = NMSNBTTagConvertedCompound

      override def valueOf[F[_]: Sync](a: NBTTagCompound): F[NMSNBTTagConvertedCompound] = for {
        a <- a.value.toList.traverse { case (k, v) => NMSNBTTag.convert(v).map(k -> _) }
        x <- NMSNBTTagCompound(a.toMap)
      } yield x
    }

    given NMSNBTTagConverter[NBTTagByte] with {
      type NMSNBTTagConvertedByte <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedByte

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagByte")
        .asInstanceOf[Class[NMSNBTTagConvertedByte]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Byte])

      override def valueOf[F[_]: Sync](a: NBTTagByte): F[NMSNBTTagConvertedByte] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagShort] with {
      type NMSNBTTagConvertedShort <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedShort

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagShort")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Short])

      override def valueOf[F[_]: Sync](a: NBTTagShort): F[NMSTagType] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagInt] with {
      type NMSNBTTagConvertedInt <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedInt

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagInt")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Int])

      override def valueOf[F[_]: Sync](a: NBTTagInt): F[NMSTagType] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagLong] with {
      type NMSNBTTagConvertedLong <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedLong

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagLong")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Long])

      override def valueOf[F[_]: Sync](a: NBTTagLong): F[NMSNBTTagConvertedLong] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagFloat] with {
      type NMSNBTTagConvertedFloat <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedFloat

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagFloat")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Float])

      override def valueOf[F[_]: Sync](a: NBTTagFloat): F[NMSNBTTagConvertedFloat] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagDouble] with {
      type NMSNBTTagConvertedDouble <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedDouble

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagDouble")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[Double])

      override def valueOf[F[_]: Sync](a: NBTTagDouble): F[NMSNBTTagConvertedDouble] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagString] with {
      type NMSNBTTagConvertedString <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedString

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagString")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[String])

      override def valueOf[F[_]: Sync](a: NBTTagString): F[NMSNBTTagConvertedString] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagByteArray] with {
      type NMSNBTTagConvertedByteArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedByteArray

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagByteArray")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[java.util.List[?]])

      override def valueOf[F[_]: Sync](a: NBTTagByteArray): F[NMSNBTTagConvertedByteArray] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagIntArray] with {
      type NMSNBTTagConvertedIntArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedIntArray

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagIntArray")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[java.util.List[?]])

      override def valueOf[F[_]: Sync](a: NBTTagIntArray): F[NMSNBTTagConvertedIntArray] = Sync[F].delay {
        valueOf.invoke(null, a.value).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagLongArray] with {
      type NMSNBTTagConvertedLongArray <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedLongArray

      private lazy val clazz   = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagLongArray")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val valueOf = ReflectionUtil
        .getMethod(clazz, "a", classOf[java.util.List[?]])

      override def valueOf[F[_]: Sync](a: NBTTagLongArray): F[NMSNBTTagConvertedLongArray] = Sync[F].delay {
        valueOf.invoke(null, a.value.asJava).asInstanceOf[NMSTagType]
      }
    }

    given NMSNBTTagConverter[NBTTagList] with {
      type NMSNBTTagConvertedList <: NMSNBTTag
      override type NMSTagType = NMSNBTTagConvertedList

      private lazy val clazz       = ReflectionUtil
        .getMinecraftClass("nbt.NBTTagList")
        .asInstanceOf[Class[NMSTagType]]
      private lazy val constructor = clazz
        .getDeclaredConstructor(classOf[java.util.List[?]], classOf[Byte])
        .tap(_.setAccessible(true))

      override def valueOf[F[_]: Sync](a: NBTTagList): F[NMSNBTTagConvertedList] = for {
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

        list <- Sync[F].delay {
          constructor.newInstance(convertedList.asJava, typeId).asInstanceOf[NMSTagType]
        }
      } yield list
    }
  }
}
