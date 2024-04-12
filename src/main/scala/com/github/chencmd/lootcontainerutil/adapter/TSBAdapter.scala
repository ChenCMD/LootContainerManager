package com.github.chencmd.lootcontainerutil.adapter

import com.github.chencmd.lootcontainerutil.Config
import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.genasset.DataSource
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemGenerator
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemIdentifier
import com.github.chencmd.lootcontainerutil.generic.EitherTExtra
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.Position
import com.github.chencmd.lootcontainerutil.nbt.NBTTagParser
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import de.tr7zw.nbtapi
import de.tr7zw.nbtapi.NBTItem
import de.tr7zw.nbtapi.NBTTileEntity
import org.bukkit.Bukkit
import org.bukkit.NamespacedKey
import org.bukkit.block.Container
import org.bukkit.inventory.ItemStack
import org.bukkit.loot.LootContext
import org.bukkit.plugin.java.JavaPlugin

object TSBAdapter {
  def createInstr[F[_]: Async](plugin: JavaPlugin, config: Config)(using
    mcThread: OnMinecraftThread[F],
    ManageItemNBT: ManageItemNBT
  ): ItemConversionInstr[F] = new ItemConversionInstr[F] {
    def toItemIdentifier(item: ItemStack): F[ItemIdentifier] = for {
      nbtItem <- mcThread.run(SyncIO(NBTItem(item)))
      item    <- NBTTagParser.parse(nbtItem.toString).fold(SystemException.raise(_), _.value.pure[F])
      tag     <- item
        .get("tag")
        .traverse(_.downcastOrRaise[NBTTag.NBTTagCompound]())
        .map(_.getOrElse(NBTTag.NBTTagCompound(Map.empty)): NBTTag.NBTTagCompound)

      (_, usingInterpolation) <- config.genAsset.toItemIdentifier
        .find(_._1.isAccessible(tag))
        .fold(ConfigurationException.raise(s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

      interpolatedTag <- usingInterpolation
        .interpolate(NBTTag.NBTTagCompound(item.updated("tag", tag)))
        .fold(ConfigurationException.raise("itemMapper did not return a result."))(_.pure[F])
    } yield interpolatedTag

    def toItemStack(item: ItemIdentifier): F[ItemStack] = {
      val server = Bukkit.getServer()
      config.genAsset.toItem
        .find(_.predicate.matches(item))
        .fold(ConfigurationException.raise(s"ItemIdentifier did not match any itemMapper. item: $item"))(_.pure[F])
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
              _   <- res.fold(ConfigurationException.raise("Failed to run preCommand."))(_ => Async[F].unit)
              rng <- Async[F].delay(java.util.Random())
              lt = server.getLootTable(NamespacedKey(plugin, g.id))
              lc = LootContext.Builder(Position(Bukkit.getWorlds.asScala.head, 0, 0, 0).toBukkit).build()
              items <- Async[F].delay(lt.populateLoot(rng, lc).asScala.toList)
              head  <-
                items.headOption.fold(ConfigurationException.raise("LootTable did not return any items."))(_.pure[F])
            } yield head
          case ItemGenerator.WithMCFunction(predicate, id, preCommand, functionOutput) => for {
              nbtDataOrErr <- mcThread.run {
                val program = for {
                  _    <- preCommand.traverse { cmd =>
                    val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), cmd))
                    EitherTExtra.exitWhenMA(res)(ConfigurationException(s"Failed to run preCommand ${cmd}."))
                  }
                  _    <- {
                    val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), s"function ${id}"))
                    EitherTExtra.exitWhenMA(res)(ConfigurationException(s"Failed to run function ${id}."))
                  }
                  item <- functionOutput match {
                    case DataSource.Block(world, x, y, z, path) => for {
                        w         <- EitherT.fromOption[SyncIO](
                          Bukkit.getWorlds.asScala.toList.find(_.getKey().toString() == world),
                          ConfigurationException(s"World ${world} was not found.")
                        )
                        container <- EitherT(SyncIO {
                          w.getBlockAt(x, y, z)
                            .getState()
                            .downcastOrLeft[Container](
                              ConfigurationException(s"Block at $x, $y, $z was not a container.")
                            )
                        })
                        blockData <- EitherT(SyncIO {
                          val s = NBTTileEntity(container).getCompound().toString()
                          NBTTagParser.parse(s).leftMap(ConfigurationException.apply)
                        })
                      } yield blockData
                    case x                                      => EitherT.liftF {
                        ConfigurationException
                          .raise[SyncIO, NBTTag.NBTTagCompound](s"DataSource ${x} is not implemented.")
                      }
                  }
                } yield item
                program.value
              }
              nbtData      <- nbtDataOrErr.fold(Async[F].raiseError, _.pure[F])
              itemData     <- {
                val isAccessible = functionOutput.path.isAccessible(nbtData)
                if (!isAccessible)
                  ConfigurationException.raise(s"Path ${functionOutput.path} did not return any items.")

                val head = functionOutput.path.access(nbtData).headOption
                if (head.isEmpty) ConfigurationException.raise(s"Path ${functionOutput.path} did not return any items.")

                head.get match {
                  case n: NBTTag.NBTTagCompound => n.pure[F]
                  case _                        => ConfigurationException.raise("Path did not return a compound.")
                }
              }

              itemStackOrErr <- mcThread.run {
                ManageItemNBT.createItemFromNBT(itemData).map(Right.apply).handleError(Left.apply)
              }
              itemStack      <- itemStackOrErr.fold(Async[F].raiseError, _.pure[F])
            } yield itemStack
        }
    }
  }
}
