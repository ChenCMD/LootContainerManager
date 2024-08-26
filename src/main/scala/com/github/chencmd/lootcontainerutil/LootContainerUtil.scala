package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.adapter.TSBAdapter
import com.github.chencmd.lootcontainerutil.adapter.database.LootAssetRepository
import com.github.chencmd.lootcontainerutil.adapter.database.LootAssetRepositoryCache
import com.github.chencmd.lootcontainerutil.adapter.database.SQLite
import com.github.chencmd.lootcontainerutil.feature.asset.ContainerManageListener
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.LootAssetHighlight
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.feature.containerprotection.ProtectActionListener
import com.github.chencmd.lootcontainerutil.generic.MapExtra.*
import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.ManageBukkitItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.OnBukkitServerThread
import com.github.chencmd.lootcontainerutil.terms.InventoriesStore
import com.github.chencmd.lootcontainerutil.terms.LootAssetCache

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.~>

import scala.concurrent.duration.*

import dev.jorel.commandapi.CommandAPI
import dev.jorel.commandapi.CommandAPIBukkitConfig
import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class LootContainerUtil extends JavaPlugin {
  type F = IO[_]
  type G = SyncIO[_]
  val coerceF: G ~> F           = FunctionK.lift([A] => (_: G[A]).to[F])
  val unsafeRunAsync            =
    (errorHandler: Throwable => F[Unit]) => [U1] => (fa: F[U1]) => fa.onError(errorHandler).unsafeRunAndForget()
  val unsafeRunSyncContinuation = [A] =>
    (cont: SyncContinuation[F, G, A]) => {
      val (a, effect) = cont.unsafeRunSync()
      effect.unsafeRunAndForget()
      a
  }

  val finalizerRef: Ref[F, Option[F[Unit]]] = Ref.unsafe(None)

  override def onLoad() = {
    CommandAPI.onLoad(CommandAPIBukkitConfig(this))
  }

  override def onEnable() = {
    val program = for {
      logger <- Slf4jLogger.create[F]
      given Logger[F] = logger

      given OnMinecraftThread[F] = OnBukkitServerThread.createInstr[F](this)
      given ManageItemNBT        = ManageBukkitItemNBT.createInstr

      cfg <- Config.tryRead[F](this)
      transactor     = SQLite.createTransactor[F](cfg.db)
      lootAssetRepos = LootAssetRepository.createInstr[F](transactor)

      given LootAssetPersistenceInstr[F] = lootAssetRepos
      given ItemConversionInstr[F]       = TSBAdapter.createInstr[F](this, cfg)

      syncLootAssetLocationCacheRef <- Ref.in[F, G, LootAssetCache](LootAssetCache.empty)
      asyncLootAssetLocationCacheRef          = syncLootAssetLocationCacheRef.mapK(coerceF)
      given LootAssetPersistenceCacheInstr[G] = LootAssetRepositoryCache.createInstr[G](syncLootAssetLocationCacheRef)
      given LootAssetPersistenceCacheInstr[F] = LootAssetRepositoryCache.createInstr[F](asyncLootAssetLocationCacheRef)

      openedInventories <- InventoriesStore.empty[F]
      pal               <- ProtectActionListener[F, G](unsafeRunSyncContinuation)
      cml               <- ContainerManageListener[F, G](openedInventories)(unsafeRunSyncContinuation)
      _                 <- Async[F].delay(Bukkit.getPluginManager.registerEvents(pal, this))
      _                 <- Async[F].delay(Bukkit.getPluginManager.registerEvents(cml, this))

      _ <- Async[F].delay(CommandAPI.onEnable())
      _ <- CommandExecutor.register[F](openedInventories, unsafeRunAsync)

      _          <- lootAssetRepos.initialize()
      _          <- refreshCache(asyncLootAssetLocationCacheRef)
      taskFiber1 <- {
        val program = for {
          cache <- asyncLootAssetLocationCacheRef.get
          _     <- Async[F].whenA(cache.updatedAssetLocations.nonEmpty || cache.deletedAssetIds.nonEmpty) {
            saveAssetFromCache(asyncLootAssetLocationCacheRef) >> refreshCache(asyncLootAssetLocationCacheRef)
          }
        } yield ()
        (Async[F].sleep(30.seconds) >> program).foreverM.start
      }

      taskFiber2 <- LootAssetHighlight.task[F].start

      _ <- finalizerRef.set(Some(for {
        _ <- taskFiber1.cancel
        _ <- taskFiber2.cancel
        _ <- saveAssetFromCache(asyncLootAssetLocationCacheRef)

        _ <- Async[F].delay(CommandAPI.onDisable())
        _ <- logger.info("LootContainerUtil disabled.")
      } yield ()))

      _ <- logger.info("LootContainerUtil enabled.")
    } yield ()

    val loggerAppliedProgram = for {
      logger <- Slf4jLogger.create[F]
      _      <- program.onError { err =>
        logger.error(err)(err.getMessage)
          >> Async[F].delay(Bukkit.getPluginManager.disablePlugin(this))
      }
    } yield ()

    loggerAppliedProgram.unsafeRunSync()
  }

  override def onDisable() = {
    finalizerRef.get.flatMap(_.orEmpty).unsafeRunSync()
  }

  def saveAssetFromCache(lootAssetLocationCacheRef: Ref[F, LootAssetCache])(using
    logger: Logger[F],
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    cache <- lootAssetLocationCacheRef.get
    _     <- logger.info("Updating assets...")
    updatingAssets = for {
      uuid <- cache.updatedAssetLocations.toList
      b    <- cache.mapping.get(uuid).toList
    } yield b
    deletedAssets  = NonEmptyList.fromList(cache.deletedAssetIds.toList)

    _ <- lootAssetRepos.upsertLootAssets(updatingAssets)
    _ <- deletedAssets.traverse_(lootAssetRepos.deleteLootAssets)
  } yield ()

  def refreshCache[F[_]: Async](lootAssetLocationCacheRef: Ref[F, LootAssetCache])(using
    logger: Logger[F],
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    _      <- logger.info("Retrieving assets...")
    assets <- lootAssetRepos.getAllLootAssets()
    assetsMap    = assets
      .flatMap(a => a.containers.map(_.location -> a.uuid))
      .groupBy(_._1.toChunkLocation)
      .mapV(_.toMap)
    assetMapping = assets.map(a => a.uuid -> a).toMap
    cache        = LootAssetCache(assetsMap, assetMapping, Set.empty, Set.empty)
    _ <- lootAssetLocationCacheRef.set(cache)

    _ <- logger.info("Assets retrieved.")
    _ <- logger.info("Assets:")
    _ <- assets.traverse_(asset => logger.info(asset.toString))
  } yield ()
}
