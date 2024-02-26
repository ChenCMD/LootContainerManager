package com.github.chencmd.lootcontainerutil.adapter

import cats.implicits.*
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.generic.EitherTExtra
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemIdentifier
import org.bukkit.inventory.ItemStack
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import cats.effect.SyncIO
import de.tr7zw.nbtapi.NBTItem
import cats.effect.kernel.Async
import com.github.chencmd.lootcontainerutil.nbt.NBTTagParser
import cats.mtl.Raise
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag
import com.github.chencmd.lootcontainerutil.Config
import org.bukkit.Bukkit
import scala.jdk.CollectionConverters.*
import cats.data.OptionT
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemGenerator
import org.bukkit.NamespacedKey
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.loot.LootContext
import com.github.chencmd.lootcontainerutil.minecraft.Location
import com.github.chencmd.lootcontainerutil.feature.genasset.DataSource
import org.bukkit.block.Container
import cats.data.EitherT
import de.tr7zw.nbtapi.NBTTileEntity
import de.tr7zw.nbtapi
import de.tr7zw.nbtapi.NBT
import org.bukkit.Material
import de.tr7zw.nbtapi.iface.ReadWriteNBT
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTNel

object TSBAdapter {
  def createInstr[F[_]: Async](plugin: JavaPlugin, config: Config)(using
    mcThread: OnMinecraftThread[F]
  ): ItemConversionInstr[F] = new ItemConversionInstr[F] {
    def toItemIdentifier(item: ItemStack)(using R: Raise[F, String]): F[ItemIdentifier] = for {
      nbtItem <- mcThread.run(SyncIO(NBTItem(item)))
      item    <- NBTTagParser.parse(nbtItem.toString).fold(R.raise, _.value.pure[F])
      tag     <- item
        .get("tag")
        .traverse(_.downcastOrRaise[NBTTag.NBTTagCompound][F]())
        .map(_.getOrElse(NBTTag.NBTTagCompound(Map.empty)): NBTTag.NBTTagCompound)

      (_, usingInterpolation) <- config.genAsset.toItemIdentifier
        .find(_._1.isAccessible(tag))
        .fold(R.raise(s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

      interpolatedTag <- usingInterpolation
        .interpolate(NBTTag.NBTTagCompound(item.updated("tag", tag)))
        .fold(R.raise("itemMapper did not return a result."))(_.pure[F])
    } yield interpolatedTag

    def toItemStack(item: ItemIdentifier)(using R: Raise[F, String]): F[ItemStack] = {
      val server = Bukkit.getServer()
      config.genAsset.toItem
        .find(_.predicate.matches(item))
        .fold(R.raise(s"ItemIdentifier did not match any itemMapper. item: $item"))(_.pure[F])
        .flatMap {
          case g: ItemGenerator.WithLootTable                                          => for {
              res <- mcThread.run {
                val program = g.preCommand.traverse_ { command =>
                  OptionT(SyncIO {
                    val res = server.dispatchCommand(Bukkit.getConsoleSender(), command)
                    Option.when(res)(())
                  })
                }
                program.value
              }
              _   <- res.fold(R.raise("Failed to run preCommand."))(_ => Async[F].unit)
              rng <- Async[F].delay(java.util.Random())
              lt = server.getLootTable(NamespacedKey(plugin, g.id))
              lc = LootContext.Builder(Location(Bukkit.getWorlds.asScala.head, 0, 0, 0).toBukkit).build()
              items <- Async[F].delay(lt.populateLoot(rng, lc).asScala.toList)
              head  <- items.headOption.fold(R.raise("LootTable did not return any items."))(_.pure[F])
            } yield head
          case ItemGenerator.WithMCFunction(predicate, id, preCommand, functionOutput) => for {
              nbtDataEither <- mcThread.run {
                val program = for {
                  _    <- preCommand.traverse { cmd =>
                    val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), cmd))
                    EitherTExtra.exitWhenMA(res)(s"Failed to run preCommand ${cmd}.")
                  }
                  _    <- {
                    val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), s"function ${id}"))
                    EitherTExtra.exitWhenMA(res)(s"Failed to run function ${id}.")
                  }
                  item <- functionOutput match {
                    case DataSource.Block(world, x, y, z, path) => for {
                        w         <- EitherT.fromOption[SyncIO](
                          Bukkit.getWorlds.asScala.toList.find(_.getKey().toString() == world),
                          s"World ${world} was not found."
                        )
                        container <- EitherT(SyncIO {
                          w.getBlockAt(x, y, z)
                            .getState()
                            .downcastOrLeft[Container]
                        })
                        blockData <- EitherT(SyncIO {
                          val s = NBTTileEntity(container).getCompound().toString()
                          NBTTagParser.parse(s)
                        })
                      } yield blockData
                    case _                                      => ???
                  }
                } yield item
                program.value
              }
              nbtData       <- nbtDataEither.fold(R.raise, _.pure[F])
              itemData      <- {
                val isAccessible = functionOutput.path.isAccessible(nbtData)
                if (!isAccessible) R.raise(s"Path ${functionOutput.path} did not return any items.")

                val head = functionOutput.path.access(nbtData).headOption
                if (head.isEmpty) R.raise(s"Path ${functionOutput.path} did not return any items.")

                head.get match {
                  case n: NBTTag.NBTTagCompound => n.pure[F]
                  case _                        => R.raise("Path did not return a compound.")
                }
              }
              itemID        <- itemData.value.get("id").fold(R.raise("Item did not have an id."))(_.pure[F])
              itemID        <- itemID.downcastOrRaise[NBTTag.NBTTagString]()
              count         <- itemData.value.get("Count").fold(R.raise("Item did not have a count."))(_.pure[F])
              count         <- count.downcastOrRaise[NBTTag.NBTTagInt]()
              itemStack = new ItemStack(Material.matchMaterial(itemID.value), count.value)
              _ <- mcThread.run(SyncIO {
                def replaceCompound(rwNBT: ReadWriteNBT, overrides: NBTTag.NBTTagCompound): Unit = {
                  overrides.value.foreach {
                    case (k, NBTTag.NBTTagString(v))     => rwNBT.setString(k, v)
                    case (k, NBTTag.NBTTagInt(v))        => rwNBT.setInteger(k, v)
                    case (k, NBTTag.NBTTagLong(v))       => rwNBT.setLong(k, v)
                    case (k, NBTTag.NBTTagShort(v))      => rwNBT.setShort(k, v)
                    case (k, NBTTag.NBTTagByte(v))       => rwNBT.setByte(k, v)
                    case (k, NBTTag.NBTTagFloat(v))      => rwNBT.setFloat(k, v)
                    case (k, NBTTag.NBTTagDouble(v))     => rwNBT.setDouble(k, v)
                    case (k, NBTTag.NBTTagByteArray(v))  => rwNBT.setByteArray(k, v.map(_.value).toArray)
                    case (k, NBTTag.NBTTagIntArray(v))   => rwNBT.setIntArray(k, v.map(_.value).toArray)
                    case (k, NBTTag.NBTTagLongArray(v))  => rwNBT.setLongArray(k, v.map(_.value).toArray)
                    case (k, NBTTag.NBTTagList(None))    => ???
                    case (k, NBTTag.NBTTagList(Some(v))) => v match {
                        case NBTNel.Byte(v)      => ???
                        case NBTNel.Short(v)     => ???
                        case NBTNel.Int(v)       => ???
                        case NBTNel.Long(v)      => ???
                        case NBTNel.String(v)    => ???
                        case NBTNel.Float(v)     => ???
                        case NBTNel.Double(v)    => ???
                        case NBTNel.ByteArray(v) => ???
                        case NBTNel.IntArray(v)  => ???
                        case NBTNel.LongArray(v) => ???
                        case NBTNel.Compound(v)  =>
                          v.toList.foreach(replaceCompound(rwNBT.getCompoundList(k).addCompound(), _))
                        case NBTNel.List(v)      => ???
                      }
                    case (k, v: NBTTag.NBTTagCompound)   => replaceCompound(rwNBT.getCompound(k), v)
                  }
                }
                NBT.modify[Unit](itemStack, replaceCompound(_, itemData))
              })
            } yield itemStack
        }
    }
  }
}
