package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.adapter.database.DBConfig
import com.github.chencmd.lootcontainerutil.feature.genasset.GenAssetConfig
import com.github.chencmd.lootcontainerutil.nbt.NBTPathInterpolationParser
import com.github.chencmd.lootcontainerutil.nbt.NBTPathParser

import cats.data.EitherNec
import cats.effect.kernel.Async
import cats.implicits.*

import scala.jdk.CollectionConverters.*

import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.java.JavaPlugin
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.*
import com.github.chencmd.lootcontainerutil.feature.genasset.DataSource
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemGenerator
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTPath
import com.github.chencmd.lootcontainerutil.feature.genasset.ItemMapper
import java.io.File

object Config {
  private def apply(genAsset: GenAssetConfig, db: DBConfig): Config = new Config(genAsset, db)

  def tryRead[F[_]: Async](plugin: JavaPlugin): F[EitherNec[String, Config]] = for {
    _          <- Async[F].delay {
      plugin.saveDefaultConfig()
      plugin.reloadConfig()
    }
    config     <- Async[F].delay(plugin.getConfig)
    dataFolder <- Async[F].delay(plugin.getDataFolder.toPath)
    genAssetConfig = getGenAssetConfig(config)
    dbConfig       = getDBConfig(config)
  } yield (genAssetConfig, dbConfig).parMapN(apply)

  def getGenAssetConfig(config: FileConfiguration): EitherNec[String, GenAssetConfig] = {
    def toItemIdentifier(sec: Map[Any, Any], i: Int): EitherNec[String, ItemMapper] = for {
      typedMap <- Either.catchNonFatal(sec.asInstanceOf[Map[String, Any]]).leftMap(_.getMessage).toEitherNec

      path = s"genAsset.toItemIdentifier[$i]"

      (from, to) <- (
        getValueWithType[String](typedMap, path)("from")
          .flatMap(s => NBTPathParser.parse(s.trim).toEitherNec),
        getValueWithType[String](typedMap, path)("to")
          .flatMap(s => NBTPathInterpolationParser.parse(s.trim).toEitherNec)
      ).parTupled
    } yield (from, to)

    def toItem(sec: Map[Any, Any], i: Int): EitherNec[String, ItemGenerator] = {
      def getDataSources(fnOut: Map[String, Any], target: String, path: NBTPath): EitherNec[String, DataSource] = {
        val p = s"genAsset.toItem[$i].functionOutput"
        target match {
          case "block"   => (
              getValueWithType[String](fnOut, p)("world"),
              getValueWithType[Double](fnOut, p)("x"),
              getValueWithType[Double](fnOut, p)("y"),
              getValueWithType[Double](fnOut, p)("z")
            ).parMapN(DataSource.Block(_, _, _, _, path))
          case "storage" => getValueWithType[String](fnOut, p)("namespace").map(DataSource.Storage(_, path))
          case "entity"  => getValueWithType[String](fnOut, p)("id").map(DataSource.Entity(_, path))
        }
      }

      val p = s"genAsset.toItem[$i]"
      for {
        typedMap <- Either.catchNonFatal(sec.asInstanceOf[Map[String, Any]]).leftMap(_.getMessage).toEitherNec

        (generateType, predicate, id, preCommand) <- (
          getValueWithType[String](typedMap, p)("generateType"),
          getValueWithType[String](typedMap, p)("predicate"),
          getValueWithType[String](typedMap, p)("id"),
          typedMap
            .getOrElse("preCommand", List.empty)
            .downcastOrLeftNec[List[Any]]
            .flatMap(_.traverse(_.downcastOrLeftNec[String]))
        ).parTupled

        itemGenerator <- generateType match {
          case "loot_table" => ItemGenerator.WithLootTable(predicate.r, id, preCommand).pure[EitherNec[String, _]]
          case "function"   => {
            val p = s"genAsset.toItem[$i].functionOutput"
            for {
              fnOut <- getValueWithType[Map[?, ?]](typedMap, p)("functionOutput")
              fnOut <- Either.catchNonFatal(fnOut.asInstanceOf[Map[String, Any]]).leftMap(_.getMessage).toEitherNec
              (target, path) <- (
                getValueWithType[String](fnOut, p)("target"),
                getValueWithType[String](fnOut, p)("path").flatMap(s => NBTPathParser.parse(s.trim).toEitherNec)
              ).parTupled

              dataSource <- getDataSources(fnOut, target, path)
            } yield ItemGenerator.WithMCFunction(predicate.r, id, preCommand, dataSource)
          }
        }
      } yield itemGenerator
    }

    for {
      genAsset       <- Option(config.getConfigurationSection("genAsset")).toRightNec("missing key 'genAsset'")
      genAssetConfig <- (
        genAsset
          .getMapList("toItemIdentifier")
          .asScala
          .toList
          .map(_.asScala.toMap)
          .zipWithIndex
          .parTraverse(toItemIdentifier.tupled),
        genAsset
          .getMapList("toItem")
          .asScala
          .toList
          .map(_.asScala.toMap)
          .zipWithIndex
          .parTraverse(toItem.tupled)
      ).parMapN(GenAssetConfig.apply)
    } yield genAssetConfig
  }

  def getDBConfig(config: FileConfiguration): EitherNec[String, DBConfig] = for {
    db       <- Option(config.getConfigurationSection("db")).toRightNec("missing key 'db'")
    dbConfig <- (
      Option(db.getString("url")).toRightNec("missing key 'db.url'"),
      Option(db.getString("user")).toRightNec("missing key 'db.user'"),
      Option(db.getString("password")).toRightNec("missing key 'db.password'"),
      Option(db.getString("filePath"))
        .toRight("missing key 'db.filePath'")
        .filterOrElse(_.endsWith(".db"), "db.filePath must have extension '.db'")
        .map(new File(_))
        .toEitherNec
    ).parMapN(DBConfig.apply)
  } yield dbConfig

  def getValueWithType[A: ClassTag: TypeTest[Any, _]](map: Map[String, Any], location: String)(
    key: String
  ): EitherNec[String, A] = for {
    value <- map.get(key).toRightNec(s"missing key '$location.$key'")
    value <- value.downcastOrLeftNec[A]
  } yield value
}

case class Config private (genAsset: GenAssetConfig, db: DBConfig)
