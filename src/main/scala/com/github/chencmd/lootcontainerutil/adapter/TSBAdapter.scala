package com.github.chencmd.lootcontainermanager.adapter

import com.github.chencmd.lootcontainermanager.Config
import com.github.chencmd.lootcontainermanager.exceptions.ConfigurationException
import com.github.chencmd.lootcontainermanager.exceptions.SystemException
import com.github.chencmd.lootcontainermanager.feature.asset.DataSource
import com.github.chencmd.lootcontainermanager.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainermanager.feature.asset.ItemGenerator
import com.github.chencmd.lootcontainermanager.feature.asset.ItemIdentifier
import com.github.chencmd.lootcontainermanager.generic.EitherTExtra
import com.github.chencmd.lootcontainermanager.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainermanager.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.nbt.NBTTagParser
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag

import cats.data.EitherT
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.Bukkit
import org.bukkit.Location
import org.bukkit.NamespacedKey
import org.bukkit.block.Container
import org.bukkit.inventory.ItemStack
import org.bukkit.loot.LootContext
import org.bukkit.plugin.java.JavaPlugin

import de.tr7zw.nbtapi.NBT
import org.typelevel.log4cats.Logger
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object TSBAdapter {
  def createInstr[F[_]: Async](plugin: JavaPlugin, config: Config)(using
    logger: Logger[F],
    mcThread: OnMinecraftThread[F],
    ManageItemNBT: ManageItemNBT
  ): ItemConversionInstr[F] = new ItemConversionInstr[F] {
    def toItemIdentifier(item: ItemStack): F[ItemIdentifier] = for {
      nbtItem <- Async[F].delay(NBT.readNbt(item))
      tag     <- NBTTagParser.parse(nbtItem.toString).fold(SystemException.raise(_), _.pure[F])
      itemTag: NBTTag.NBTTagCompound = NBTTag.NBTTagCompound(
        Map(
          "id"    -> NBTTag.NBTTagString(item.getType().getKey().toString()),
          "Count" -> NBTTag.NBTTagByte(item.getAmount().toByte),
          "tag"   -> tag
        )
      )

      (_, usingInterpolation) <- config.genAsset.toItemIdentifier
        .find(_._1.isAccessible(itemTag))
        .fold(ConfigurationException.raise(s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

      interpolatedTag <- usingInterpolation
        .interpolate(itemTag)
        .fold(ConfigurationException.raise("itemMapper did not return a result."))(_.pure[F])
    } yield interpolatedTag

    def toItemStack(item: ItemIdentifier): F[ItemStack] = for {
      generator <- config.genAsset.toItem
        .find(_.predicate.matches(item))
        .fold(ConfigurationException.raise(s"ItemIdentifier did not match any itemMapper. item: $item"))(_.pure[F])

      matched     = generator.predicate.findFirstMatchIn(item).get
      interp      = interpolate(matched)
      id          = interp(generator.id)
      preCommands = generator.preCommands.map(interp)

      item <- generator match {
        case g: ItemGenerator.WithItemId => for {
            res <- mcThread.run(runPreCommands(preCommands).value)
            _   <- res.fold(Async[F].raiseError, _ => Async[F].unit)

            tag <- g.tag.map(interp).filter(_.nonEmpty).traverse { t =>
              val res = NBTTagParser.parse(t)
              logger.info(t) >>
              res.fold(ConfigurationException.raise, _.pure[F])
            }

            item <- ManageItemNBT.createItemFromNBT[F](
              NBTTag.NBTTagCompound(
                Map(
                  "id"    -> NBTTag.NBTTagString(id),
                  "Count" -> NBTTag.NBTTagByte(1),
                  "tag"   -> tag.getOrElse(NBTTag.NBTTagCompound(Map.empty))
                )
              )
            )
          } yield item

        case g: ItemGenerator.WithLootTable => for {
            res <- mcThread.run(runPreCommands(preCommands).value)
            _   <- res.fold(Async[F].raiseError, _ => Async[F].unit)

            lt <- Async[F].delay(Bukkit.getServer.getLootTable(NamespacedKey.fromString(id)))
            _ <- logger.info(s"Using loot table: $lt, id: $id")

            lc = LootContext.Builder(new Location(Bukkit.getWorlds.asScala.head, 0, 0, 0)).build()

            rng   <- Async[F].delay(java.util.Random())
            items <- Async[F].delay(lt.populateLoot(rng, lc).asScala.toList)
            res = items.headOption
            item <- res.fold(ConfigurationException.raise("LootTable did not return any items."))(_.pure[F])
          } yield item

        case g: ItemGenerator.WithMCFunction => for {
            item <- mcThread.run {
              val program = for {
                _ <- runPreCommands(g.preCommands.map(interp))
                _ <- runCommand(s"execute as @p run function $id")(
                  ConfigurationException(s"Failed to run function g.${id}.")
                )

                fnOut = g.functionOutput

                nbtData <- generateItemCompoundFromDataSource(fnOut)

                itemData <- EitherT.fromEither[SyncIO](g.functionOutput.path.access(nbtData).headOption match {
                  case Some(n: NBTTag.NBTTagCompound) => Right(n)
                  case Some(_) => Left(ConfigurationException(s"Path ${fnOut.path} did not return a compound."))
                  case None    => Left(ConfigurationException(s"Path ${fnOut.path} did not return any items."))
                })

                item <- ManageItemNBT.createItemFromNBT[SyncIO](itemData).attemptT
              } yield item
              program.value
            }
            item <- item.fold(Async[F].raiseError, _.pure[F])
          } yield item
      }
    } yield item

    def generateItemCompoundFromDataSource(
      dataSource: DataSource
    ): EitherT[SyncIO, ConfigurationException, NBTTag.NBTTagCompound] = dataSource match {
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

      case x => EitherT.liftF {
          ConfigurationException.raise[SyncIO, NBTTag.NBTTagCompound](s"DataSource ${x} is not implemented.")
        }
    }

    def interpolate(matched: Match)(str: String) = {
      val str2 = raw"%%(\d+)%%".r
        .replaceAllIn(str, m => Regex.quoteReplacement(Option(matched.group(m.group(1).toInt)).orEmpty))
      val str3 = raw"%%(\w+)%%".r
        .replaceAllIn(str2, m => Regex.quoteReplacement(Option(matched.group(m.group(1))).orEmpty))
      str3
    }

    def runPreCommands(commands: List[String]): EitherT[SyncIO, ConfigurationException, Unit] = {
      commands.traverse_ { cmd =>
        runCommand(cmd)(ConfigurationException(s"Failed to run preCommand: $cmd."))
      }
    }

    def runCommand[E](cmd: String)(handleError: => E): EitherT[SyncIO, E, Unit] = {
      val res = SyncIO(Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), cmd))
      EitherTExtra.exitWhenMA(res)(handleError)
    }
  }
}
