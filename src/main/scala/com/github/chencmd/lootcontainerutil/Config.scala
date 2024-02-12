package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.nbt.NBTPathInterpolationParser
import com.github.chencmd.lootcontainerutil.nbt.NBTPathParser
import com.github.chencmd.lootcontainerutil.terms.ItemMapper

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.java.JavaPlugin
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*

object Config {
  private def apply(itemMappers: List[ItemMapper]): Config = new Config(itemMappers)

  def tryRead[F[_]: Async](plugin: JavaPlugin): F[Either[NonEmptyChain[String], Config]] = for {
    _      <- Async[F].delay {
      plugin.saveDefaultConfig()
      plugin.reloadConfig()
    }
    config <- Async[F].delay(plugin.getConfig)
    itemMappers = getItemMapper(config)
  } yield itemMappers.leftMap(NonEmptyChain.one(_)).map(apply).toEither

  def getItemMapper(config: FileConfiguration): Validated[String, List[ItemMapper]] = {
    config
      .getMapList("itemMap")
      .asScala
      .toList
      .map(_.asScala.toMap)
      .mapWithIndex {
        case (m, i) => for {
            typedMap <- {
              try m.asInstanceOf[Map[String, Any]].asRight
              catch e => Either.left(e.getMessage)
            }
            from  <- typedMap.get("from").toRight(s"missing key 'itemMap[$i].from'").flatMap(_.downcastOrLeft[String])
            from  <- NBTPathParser.parse(from.trim)
            rawTo <- for {
              anyValue        <- typedMap.get("to").toRight(s"missing key 'itemMap[$i].to'")
              anyBufferValue  <- anyValue.downcastOrLeft[java.util.List[?]].map(_.asScala.toList)
              stringListValue <- anyBufferValue.toList.traverseWithIndexM { (s, j) =>
                s.downcastOrNone[String].toRight(s"bad value type 'itemMap[$i].to[$j]', string expected")
              }
            } yield stringListValue
            to    <- rawTo.traverse(t => NBTPathInterpolationParser.parse(t.trim))
          } yield (from, to)
      }
      .traverse(_.toValidated)
  }
}

case class Config private (itemMappers: List[ItemMapper])
