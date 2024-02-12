package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.nbt.NBTPathInterpolationParser
import com.github.chencmd.lootcontainerutil.nbt.NBTPathParser
import com.github.chencmd.lootcontainerutil.terms.ItemMapper

import cats.data.EitherNec
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.java.JavaPlugin
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import java.nio.file.Path

object Config {
  private def apply(itemMappers: List[ItemMapper], spreadsheetId: String, googleCredentialsPath: Path): Config =
    new Config(itemMappers, spreadsheetId, googleCredentialsPath)

  def tryRead[F[_]: Async](plugin: JavaPlugin): F[EitherNec[String, Config]] = for {
    _          <- Async[F].delay {
      plugin.saveDefaultConfig()
      plugin.reloadConfig()
    }
    config     <- Async[F].delay(plugin.getConfig)
    dataFolder <- Async[F].delay(plugin.getDataFolder.toPath)
    itemMappers           = getItemMapper(config)
    spreadsheetId         = getSpreadsheetId(config)
    googleCredentialsPath = getGoogleCredentialsPath(config, dataFolder)
  } yield (itemMappers, spreadsheetId, googleCredentialsPath).parMapN(apply)

  def getItemMapper(config: FileConfiguration): EitherNec[String, List[ItemMapper]] = {
    config
      .getMapList("itemMap")
      .asScala
      .toList
      .map(_.asScala.toMap)
      .traverseWithIndexM {
        case (m, i) => for {
            typedMap <- {
              try m.asInstanceOf[Map[String, Any]].asRight
              catch e => Either.leftNec(e.getMessage)
            }
            from     <-
              typedMap.get("from").toRightNec(s"missing key 'itemMap[$i].from'").flatMap(_.downcastOrLeftNec[String])
            from     <- NBTPathParser.parse(from.trim).toEitherNec
            rawTo    <- for {
              anyValue        <- typedMap.get("to").toRightNec(s"missing key 'itemMap[$i].to'")
              anyBufferValue  <- anyValue.downcastOrLeftNec[java.util.List[?]].map(_.asScala.toList)
              stringListValue <- anyBufferValue.toList.traverseWithIndexM { (s, j) =>
                s.downcastOrNone[String].toRightNec(s"bad value type 'itemMap[$i].to[$j]', string expected")
              }
            } yield stringListValue
            to       <- rawTo.traverse(t => NBTPathInterpolationParser.parse(t.trim).toEitherNec)
          } yield (from, to)
      }
  }

  def getSpreadsheetId(config: FileConfiguration): EitherNec[String, String] = {
    Option(config.getString("spreadsheetId")).toRightNec("missing key 'spreadsheetId'")
  }

  def getGoogleCredentialsPath(config: FileConfiguration, dataFolder: Path): EitherNec[String, Path] = {
    Option(config.getString("googleCredentialsPath"))
      .map(dataFolder.resolve)
      .toRightNec("missing key 'googleCredentialsPath'")
  }
}

case class Config private (itemMappers: List[ItemMapper], spreadsheetId: String, googleCredentialsPath: Path)
