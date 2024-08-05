package com.github.chencmd.lootcontainerutil.adapter

import com.github.chencmd.lootcontainerutil.Config
import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import com.github.chencmd.lootcontainerutil.exceptions.SystemException
import com.github.chencmd.lootcontainerutil.feature.asset.DataSource
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.ItemGenerator
import com.github.chencmd.lootcontainerutil.feature.asset.ItemIdentifier
import com.github.chencmd.lootcontainerutil.generic.EitherTExtra
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.nbt.NBTTagParser
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import de.tr7zw.nbtapi.NBT
import org.bukkit.Bukkit
import org.bukkit.Location
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
      nbtItem <- Async[F].delay(NBT.readNbt(item))
      tag     <- NBTTagParser.parse(nbtItem.toString).fold(SystemException.raise(_), _.pure[F])

      (_, usingInterpolation) <- config.genAsset.toItemIdentifier
        .find(_._1.isAccessible(tag))
        .fold(ConfigurationException.raise(s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

      itemTag = Map(
        "id"    -> NBTTag.NBTTagString(item.getType().getKey().toString()),
        "Count" -> NBTTag.NBTTagByte(item.getAmount().toByte),
        "tag"   -> tag
      )
      interpolatedTag <- usingInterpolation
        .interpolate(NBTTag.NBTTagCompound(itemTag))
        .fold(ConfigurationException.raise("itemMapper did not return a result."))(_.pure[F])
    } yield interpolatedTag

    def toItemStack(item: ItemIdentifier): F[ItemStack] = {
      val server = Bukkit.getServer()
      config.genAsset.toItem
        .find(_.predicate.matches(item))
        .fold(ConfigurationException.raise(s"ItemIdentifier did not match any itemMapper. item: $item"))(_.pure[F])
        .flatMap { generator =>
          val matched                  = generator.predicate.findFirstMatchIn(item).get
          def interpolate(str: String) = {
            val str2 = raw"%%(\d+)%%".r.replaceAllIn(str, m => Option(matched.group(m.group(1).toInt)).orEmpty)
            val str3 = raw"%%(\w+)%%".r.replaceAllIn(str2, m => Option(matched.group(m.group(1))).orEmpty)
            str3
          }

          val id = interpolate(generator.id)

          generator match {
            case g: ItemGenerator.WithItemId => ManageItemNBT
                .createItemFromNBT[SyncIO](
                  NBTTag.NBTTagCompound(
                    Map(
                      "id"    -> NBTTag.NBTTagString(id),
                      "Count" -> NBTTag.NBTTagByte(1),
                      "tag"   -> NBTTag.NBTTagCompound(Map.empty)
                    )
                  )
                )
                .to[F]

            case g: ItemGenerator.WithLootTable => for {
                res <- mcThread.run {
                  val program = g.preCommand.traverse_ { command =>
                    OptionT(SyncIO {
                      val res = server.dispatchCommand(Bukkit.getConsoleSender(), interpolate(command))
                      Option.when(res)(())
                    })
                  }
                  program.value
                }
                _   <- res.fold(ConfigurationException.raise("Failed to run preCommand."))(_ => Async[F].unit)
                rng <- Async[F].delay(java.util.Random())
                lt = server.getLootTable(NamespacedKey(plugin, id))
                lc = LootContext.Builder(new Location(Bukkit.getWorlds.asScala.head, 0, 0, 0)).build()
                items <- Async[F].delay(lt.populateLoot(rng, lc).asScala.toList)
                head  <-
                  items.headOption.fold(ConfigurationException.raise("LootTable did not return any items."))(_.pure[F])
              } yield head

            case g: ItemGenerator.WithMCFunction => for {
                nbtData <- attemptGenerateFromMCFunction(g, id)
                _       <- g.functionOutput.path.isAccessible(nbtData) match {
                  case true  => Async[F].unit
                  case false => ConfigurationException.raise(s"Path ${g.functionOutput.path} did not return any items.")
                }

                itemData <- g.functionOutput.path.access(nbtData).headOption match {
                  case Some(n: NBTTag.NBTTagCompound) => n.pure[F]
                  case Some(_)                        =>
                    ConfigurationException.raise(s"Path ${g.functionOutput.path} did not return a compound.")
                  case None => ConfigurationException.raise(s"Path ${g.functionOutput.path} did not return any items.")
                }

                itemStackOrErr <- mcThread.run {
                  ManageItemNBT.createItemFromNBT[SyncIO](itemData).map(Right.apply).handleError(Left.apply)
                }
                itemStack      <- itemStackOrErr.fold(Async[F].raiseError, _.pure[F])
              } yield itemStack
          }
        }
    }

    def attemptGenerateFromMCFunction(g: ItemGenerator.WithMCFunction, id: String): F[NBTTag.NBTTagCompound] = {
      val server = Bukkit.getServer()
      val res    = mcThread.run {
        val program = for {
          _    <- g.preCommand.traverse { cmd =>
            val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), cmd))
            EitherTExtra.exitWhenMA(res)(ConfigurationException(s"Failed to run preCommand ${cmd}."))
          }
          _    <- {
            val res = SyncIO(server.dispatchCommand(Bukkit.getConsoleSender(), s"function ${id}"))
            EitherTExtra.exitWhenMA(res)(ConfigurationException(s"Failed to run function g.${id}."))
          }
          item <- g.functionOutput match {
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
                  val s = NBT.get[String](container, _.toString())
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
      res.flatMap(_.fold(Async[F].raiseError, _.pure[F]))
    }
  }
}
