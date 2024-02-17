package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.database.DBConfig
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

object Config {
  private def apply(itemMappers: List[ItemMapper], db: DBConfig): Config = new Config(itemMappers, db)

  def tryRead[F[_]: Async](plugin: JavaPlugin): F[EitherNec[String, Config]] = for {
    _          <- Async[F].delay {
      plugin.saveDefaultConfig()
      plugin.reloadConfig()
    }
    config     <- Async[F].delay(plugin.getConfig)
    dataFolder <- Async[F].delay(plugin.getDataFolder.toPath)
    itemMappers = getItemMapper(config)
    dbConfig    = getDBConfig(config)
  } yield (itemMappers, dbConfig).parMapN(apply)

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
      .toEitherNec
  }

  def getDBConfig(config: FileConfiguration): EitherNec[String, DBConfig] = for {
    db       <- Option(config.getConfigurationSection("db")).toRightNec("missing key 'db'")
    url      <- Option(db.getString("url")).toRightNec("missing key 'db.url'")
    user     <- Option(db.getString("user")).toRightNec("missing key 'db.user'")
    password <- Option(db.getString("password")).toRightNec("missing key 'db.password'")
  } yield DBConfig(url, user, password)
}

case class Config private (itemMappers: List[ItemMapper], db: DBConfig)
