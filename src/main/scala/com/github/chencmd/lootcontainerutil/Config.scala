package com.github.chencmd.lootcontainermanager

import com.github.chencmd.lootcontainermanager.adapter.database.DBConfig
import com.github.chencmd.lootcontainermanager.exceptions.ConfigurationException
import com.github.chencmd.lootcontainermanager.feature.asset.AssetConfig
import com.github.chencmd.lootcontainermanager.feature.asset.DataSource
import com.github.chencmd.lootcontainermanager.feature.asset.ItemGenerator
import com.github.chencmd.lootcontainermanager.feature.asset.ItemMapper
import com.github.chencmd.lootcontainermanager.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainermanager.nbt.NBTPathInterpolationParser
import com.github.chencmd.lootcontainermanager.nbt.NBTPathParser
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPath

import cats.data.EitherNec
import cats.effect.kernel.Async
import cats.implicits.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import scala.util.chaining.*

import java.nio.file.Path

import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.java.JavaPlugin

object Config {
  private def apply(asset: AssetConfig, db: DBConfig, debug: Boolean): Config = new Config(asset, db, debug)

  def tryRead[F[_]: Async](plugin: JavaPlugin): F[Config] = for {
    _          <- Async[F].delay {
      plugin.saveDefaultConfig()
      plugin.reloadConfig()
    }
    config     <- Async[F].delay(plugin.getConfig)
    dataFolder <- Async[F].delay(plugin.getDataFolder.toPath)
    assetConfig = getAssetConfig(config)
    dbConfig    = getDBConfig(config, dataFolder)
    debug       = config.getBoolean("debug", false).rightNec
    config <- (assetConfig, dbConfig, debug)
      .parMapN(Config.apply)
      .fold(s => ConfigurationException.raise(s.mkString_("\n")), _.pure[F])
  } yield config

  def getAssetConfig(config: FileConfiguration): EitherNec[String, AssetConfig] = {
    def toItemIdentifier(sec: Map[Any, Any], i: Int): EitherNec[String, ItemMapper] = for {
      typedMap <- Either.catchNonFatal(sec.asInstanceOf[Map[String, Any]]).leftMap(_.getMessage).toEitherNec

      path = s"asset.toItemIdentifier[$i]"

      (predicate, to) <- (
        getValueWithType[String](typedMap, path)("predicate")
          .flatMap(s => NBTPathParser.parse(s.trim).toEitherNec),
        getValueWithType[String](typedMap, path)("to")
          .flatMap(s => NBTPathInterpolationParser.parse(s.trim).toEitherNec)
      ).parTupled
    } yield (predicate, to)

    def toItem(sec: Map[Any, Any], i: Int): EitherNec[String, ItemGenerator] = {
      def getDataSources(fnOut: Map[String, Any], target: String, path: NBTPath): EitherNec[String, DataSource] = {
        val p = s"asset.toItem[$i].functionOutput"
        target match {
          case "block"   => (
              getValueWithType[String](fnOut, p)("world"),
              getValueWithType[Int](fnOut, p)("x"),
              getValueWithType[Int](fnOut, p)("y"),
              getValueWithType[Int](fnOut, p)("z")
            ).parMapN(DataSource.Block(_, _, _, _, path))
          case "storage" => getValueWithType[String](fnOut, p)("namespace").map(DataSource.Storage(_, path))
          case "entity"  => getValueWithType[String](fnOut, p)("id").map(DataSource.Entity(_, path))
          case _ => Either.leftNec(s"Invalid target type '$target' in $p expected 'block', 'storage', or 'entity'")
        }
      }

      val p = s"asset.toItem[$i]"
      for {
        typedMap <- Either.catchNonFatal(sec.asInstanceOf[Map[String, Any]]).leftMap(_.getMessage).toEitherNec

        (generateType, predicate, id, preCommands) <- (
          getValueWithType[String](typedMap, p)("generateType"),
          getValueWithType[String](typedMap, p)("predicate"),
          getValueWithType[String](typedMap, p)("id"),
          typedMap
            .getOrElse("preCommands", java.util.ArrayList[String]())
            .downcastOrLeftNec[java.util.ArrayList[?]]
            .flatMap(_.asScala.toList.traverse(_.asInstanceOf[Any].downcastOrLeftNec[String]))
        ).parTupled

        itemGenerator <- generateType match {
          case "loot_table" => ItemGenerator.WithLootTable(predicate.r, id, preCommands).pure[EitherNec[String, _]]
          case "function"   => {
            val p = s"asset.toItem[$i].functionOutput"
            for {
              fnOut          <- getValueWithType[java.util.Map[?, ?]](typedMap, p)("functionOutput")
              fnOut          <- Either
                .catchNonFatal(fnOut.asScala.toMap.asInstanceOf[Map[String, Any]])
                .leftMap(_.getMessage)
                .toEitherNec
              (target, path) <- (
                getValueWithType[String](fnOut, p)("target"),
                getValueWithType[String](fnOut, p)("path").flatMap(s => NBTPathParser.parse(s.trim).toEitherNec)
              ).parTupled

              dataSource <- getDataSources(fnOut, target, path)
            } yield ItemGenerator.WithMCFunction(predicate.r, id, preCommands, dataSource)
          }
          case "give"       => {
            val tag = getValueWithType[String](typedMap, p)("tag").toOption
            ItemGenerator.WithItemId(predicate.r, id, tag, preCommands).pure[EitherNec[String, _]]
          }
          case _ => Either.leftNec(s"Invalid generateType '$generateType' in $p expected 'loot_table' or 'function'")
        }
      } yield itemGenerator
    }

    for {
      asset       <- Option(config.getConfigurationSection("asset")).toRightNec("missing key 'asset'")
      AssetConfig <- (
        asset
          .getLong("highlightRefreshInterval", 3)
          .pipe(_.seconds)
          .rightNec,
        asset
          .getMapList("toItemIdentifier")
          .asScala
          .toList
          .map(_.asScala.toMap)
          .zipWithIndex
          .parTraverse(toItemIdentifier.tupled),
        asset
          .getMapList("toItem")
          .asScala
          .toList
          .map(_.asScala.toMap)
          .zipWithIndex
          .parTraverse(toItem.tupled)
      ).parMapN(AssetConfig.apply)
    } yield AssetConfig
  }

  def getDBConfig(config: FileConfiguration, dataFolder: Path): EitherNec[String, DBConfig] = for {
    db                         <- Option(config.getConfigurationSection("db")).toRightNec("missing key 'db'")
    filePath                   <- Option(db.getString("filePath"))
      .toRight("missing key 'db.filePath'")
      .filterOrElse(_.endsWith(".db"), "db.filePath must have extension '.db'")
      .map(dataFolder.resolve(_).toFile)
      .toEitherNec
    attemptSaveIntervalSeconds <- db
      .getLong("attemptSaveIntervalSeconds", 30)
      .pipe(_.seconds)
      .rightNec
  } yield DBConfig(filePath, attemptSaveIntervalSeconds)

  def getValueWithType[A: ClassTag: TypeTest[Any, _]](map: Map[String, Any], location: String)(
    key: String
  ): EitherNec[String, A] = for {
    value <- map.get(key).toRightNec(s"missing key '$location.$key'")
    value <- value.downcastOrLeftNec[A]
  } yield value
}

case class Config private (asset: AssetConfig, db: DBConfig, debug: Boolean)
