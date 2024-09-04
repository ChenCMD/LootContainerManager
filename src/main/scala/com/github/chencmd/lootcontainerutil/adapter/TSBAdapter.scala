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
import com.github.chencmd.lootcontainermanager.nbt.NBTTagParser
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.VoidCommandSender

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Sync
import cats.implicits.*

import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

import org.bukkit.Bukkit
import org.bukkit.GameRule
import org.bukkit.Location
import org.bukkit.NamespacedKey
import org.bukkit.block.Container
import org.bukkit.inventory.ItemStack
import org.bukkit.loot.LootContext
import org.bukkit.plugin.java.JavaPlugin

import com.github.tarao.record4s.ArrayRecord
import de.tr7zw.nbtapi.NBT

object TSBAdapter {
  def createInstr[F[_]: Async, G[_]: Sync](plugin: JavaPlugin, config: Config)(using
    ManageItemNBT: ManageItemNBT
  ): ItemConversionInstr[F, G] = new ItemConversionInstr[F, G] {
    def toItemIdentifier(item: ItemStack): F[ItemIdentifier] = for {
      nbtItem <- Async[F].delay(NBT.readNbt(item))
      tag     <- NBTTagParser.parse(nbtItem.toString).fold(SystemException.raise[F](_), _.pure[F])
      itemTag: NBTTag.NBTTagCompound = NBTTag.NBTTagCompound(
        Map(
          "id"    -> NBTTag.NBTTagString(item.getType().getKey().toString()),
          "Count" -> NBTTag.NBTTagByte(item.getAmount().toByte),
          "tag"   -> tag
        )
      )

      (_, usingInterpolation) <- config.asset.toItemIdentifier
        .find(_._1.isAccessible(itemTag))
        .fold(ConfigurationException.raise[F](s"A matched itemMapper was not found. data: ${tag.toSNBT}"))(_.pure[F])

      interpolatedTag <- usingInterpolation
        .interpolate(itemTag)
        .fold(ConfigurationException.raise[F]("itemMapper did not return a result."))(_.pure[F])
    } yield interpolatedTag

    def toItemStack(item: ItemIdentifier): G[ItemStack] = for {
      generator <- config.asset.toItem
        .find(_.predicate.matches(item))
        .fold(ConfigurationException.raise[G](s"ItemIdentifier did not match any itemMapper. item: $item"))(_.pure[G])

      matched     = generator.predicate.findFirstMatchIn(item).get
      interp      = interpolate(matched)
      id          = interp(generator.id)
      preCommands = generator.preCommands.map(interp)

      item <- generator match {
        case g: ItemGenerator.WithItemId => for {
            res <- muteCommandLogWith { runPreCommands(preCommands) }.value
            _   <- res.fold(Sync[G].raiseError, _ => Sync[G].unit)

            tag <- g.tag.map(interp).filter(_.nonEmpty).traverse { t =>
              val res = NBTTagParser.parse(t)
              res.fold(ConfigurationException.raise[G](_), _.pure[G])
            }

            item <- ManageItemNBT.createItemFromNBT[G](
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
            res <- muteCommandLogWith { runPreCommands(preCommands) }.value
            _   <- res.fold(Sync[G].raiseError, _ => Sync[G].unit)

            lt <- Sync[G].delay(Bukkit.getServer.getLootTable(NamespacedKey.fromString(id)))

            lc = LootContext.Builder(new Location(Bukkit.getWorlds.asScala.head, 0, 0, 0)).build()

            rng   <- Sync[G].delay(java.util.Random())
            items <- Sync[G].delay(lt.populateLoot(rng, lc).asScala.toList)
            res = items.headOption
            item <- res.fold(ConfigurationException.raise[G]("LootTable did not return any items."))(_.pure[G])
          } yield item

        case g: ItemGenerator.WithMCFunction => for {
            item <- {
              val program = for {
                _ <- muteCommandLogWith {
                  runPreCommands(g.preCommands.map(interp))
                    >> runCommand(s"execute as @p run function $id")(
                      ConfigurationException(s"Failed to run function g.${id}.")
                    )
                }
                fnOut = g.functionOutput

                nbtData <- generateItemCompoundFromDataSource(fnOut)

                itemData <- EitherT.fromEither[G](g.functionOutput.path.access(nbtData).headOption match {
                  case Some(n: NBTTag.NBTTagCompound) => Right(n)
                  case Some(_) => Left(ConfigurationException(s"Path ${fnOut.path} did not return a compound."))
                  case None    => Left(ConfigurationException(s"Path ${fnOut.path} did not return any items."))
                })

                item <- ManageItemNBT.createItemFromNBT[G](itemData).attemptT
              } yield item
              program.value
            }
            item <- item.fold(Sync[G].raiseError, _.pure[G])
          } yield item
      }
    } yield item

    def generateItemCompoundFromDataSource(
      dataSource: DataSource
    ): EitherT[G, ConfigurationException, NBTTag.NBTTagCompound] = dataSource match {
      case DataSource.Block(world, x, y, z, path) => for {
          w         <- EitherT.fromOption[G](
            Bukkit.getWorlds.asScala.toList.find(_.getKey().toString() == world),
            ConfigurationException(s"World ${world} was not found.")
          )
          container <- EitherT(Sync[G].delay {
            w.getBlockAt(x, y, z)
              .getState()
              .downcastOrLeft[Container](
                ConfigurationException(s"Block at $x, $y, $z was not a container.")
              )
          })
          blockData <- EitherT(Sync[G].delay {
            val s = NBT.get[String](container, _.toString())
            NBTTagParser.parse(s).leftMap(ConfigurationException.apply)
          })
        } yield blockData

      case x => EitherT.liftF {
          ConfigurationException.raise[G](s"DataSource ${x} is not implemented.")
        }
    }

    def interpolate(matched: Match)(str: String) = {
      val str2 = raw"%%(\d+)%%".r
        .replaceAllIn(str, m => Regex.quoteReplacement(Option(matched.group(m.group(1).toInt)).orEmpty))
      val str3 = raw"%%(\w+)%%".r
        .replaceAllIn(str2, m => Regex.quoteReplacement(Option(matched.group(m.group(1))).orEmpty))
      str3
    }

    def muteCommandLogWith[E <: Throwable](f: => EitherT[G, E, Unit]): EitherT[G, Throwable, Unit] = for {
      worlds           <- EitherT.liftF(Sync[G].delay(Bukkit.getWorlds.asScala.toList))
      beforeRuleValues <- EitherT.liftF(worlds.traverse { w =>
        for {
          scf <- Sync[G].delay(w.getGameRuleValue(GameRule.SEND_COMMAND_FEEDBACK))
          lac <- Sync[G].delay(w.getGameRuleValue(GameRule.LOG_ADMIN_COMMANDS))
        } yield w -> ArrayRecord(scf = scf, lac = lac)
      })
      _                <- EitherT.liftF(beforeRuleValues.traverse { (w, v) =>
        for {
          _ <- Sync[G].whenA(v.scf)(Sync[G].delay(w.setGameRule(GameRule.SEND_COMMAND_FEEDBACK, false)))
          _ <- Sync[G].whenA(v.lac)(Sync[G].delay(w.setGameRule(GameRule.LOG_ADMIN_COMMANDS, false)))
        } yield ()
      })
      _                <- f.leftWiden
      _                <- EitherT.liftF(beforeRuleValues.traverse_ { (w, v) =>
        for {
          _ <- Sync[G].delay(w.setGameRule(GameRule.SEND_COMMAND_FEEDBACK, v.scf))
          _ <- Sync[G].delay(w.setGameRule(GameRule.LOG_ADMIN_COMMANDS, v.lac))
        } yield ()
      })
    } yield ()

    def runPreCommands(commands: List[String]): EitherT[G, ConfigurationException, Unit] = {
      commands.traverse_ { cmd =>
        runCommand(cmd)(ConfigurationException(s"Failed to run preCommand: $cmd."))
      }
    }

    def runCommand[E](cmd: String)(handleError: => E): EitherT[G, E, Unit] = {
      val res = for {
        voidSender <- VoidCommandSender.createVoidSender[G]
        res        <- Sync[G].delay(Bukkit.getServer().dispatchCommand(voidSender, cmd))
      } yield res
      EitherTExtra.exitWhenMA(res)(handleError)
    }
  }
}
