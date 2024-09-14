package com.github.chencmd.lootcontainermanager

import com.github.chencmd.lootcontainermanager.adapter.TSBAdapter
import com.github.chencmd.lootcontainermanager.adapter.database.LootAssetRepository
import com.github.chencmd.lootcontainermanager.adapter.database.LootAssetRepositoryCache
import com.github.chencmd.lootcontainermanager.adapter.database.SQLite
import com.github.chencmd.lootcontainermanager.feature.asset.ContainerManageListener
import com.github.chencmd.lootcontainermanager.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainermanager.feature.asset.LootAssetHighlight
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainermanager.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainermanager.feature.containerprotection.ProtectActionListener
import com.github.chencmd.lootcontainermanager.generic.extensions.MapExt.*
import com.github.chencmd.lootcontainermanager.generic.SyncContinuation
import com.github.chencmd.lootcontainermanager.generic.extensions.IOExt.*
import com.github.chencmd.lootcontainermanager.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.ManageBukkitItemNBT
import com.github.chencmd.lootcontainermanager.minecraft.bukkit.OnBukkitServerThread
import com.github.chencmd.lootcontainermanager.terms.InventoriesStore
import com.github.chencmd.lootcontainermanager.terms.LootAssetCache

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.~>
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin

import dev.jorel.commandapi.CommandAPI
import dev.jorel.commandapi.CommandAPIBukkitConfig

class LootContainerManager extends JavaPlugin {
  type F = IO[_]
  type G = SyncIO[_]
  val coerceF: G ~> F           = FunctionK.lift([A] => (_: G[A]).to[F])
  val unsafeRunAsync            = [U1] => (fa: F[U1]) => fa.unsafeRunAndForget()
  val unsafeRunSyncG            = [A] => (fa: G[A]) => fa.unsafeRunSync()
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

      given OnMinecraftThread[F, G] = OnBukkitServerThread.createInstr[F, G](this)(unsafeRunSyncG, coerceF)
      given ManageItemNBT           = ManageBukkitItemNBT.createInstr

      cfg <- Config.tryRead[F](this)
      transactor = SQLite.createTransactor[F](cfg.db, cfg.debug)
      _ <- SQLite.migrate[F](classOf[LootContainerManager], cfg.db)
      lootAssetRepos = LootAssetRepository.createInstr[F](transactor)

      given LootAssetPersistenceInstr[F] = lootAssetRepos
      given ItemConversionInstr[F, G]    = TSBAdapter.createInstr[F, G](this, cfg)

      syncLootAssetLocationCacheRef <- Ref.in[F, G, LootAssetCache](LootAssetCache.empty)
      asyncLootAssetLocationCacheRef          = syncLootAssetLocationCacheRef.mapK(coerceF)
      given LootAssetPersistenceCacheInstr[G] = LootAssetRepositoryCache.createInstr[G](syncLootAssetLocationCacheRef)
      given LootAssetPersistenceCacheInstr[F] = LootAssetRepositoryCache.createInstr[F](asyncLootAssetLocationCacheRef)

      openedInventories <- InventoriesStore.empty[F]
      pal               <- ProtectActionListener[F, G](unsafeRunSyncContinuation)
      cml               <- ContainerManageListener[F, G](openedInventories)(unsafeRunSyncContinuation, cfg.debug)
      _                 <- Async[F].delay(Bukkit.getPluginManager.registerEvents(pal, this))
      _                 <- Async[F].delay(Bukkit.getPluginManager.registerEvents(cml, this))

      _ <- Async[F].delay(CommandAPI.onEnable())
      _ <- CommandExecutor.register[F, G](openedInventories, unsafeRunAsync, cfg.debug)

      _          <- refreshCache(asyncLootAssetLocationCacheRef, cfg.debug)
      taskFiber1 <- {
        val program = for {
          cache <- asyncLootAssetLocationCacheRef.get
          _     <- Async[F].whenA(cache.updatedAssetLocations.nonEmpty || cache.deletedAssetIds.nonEmpty) {
            saveAssetFromCache(asyncLootAssetLocationCacheRef, cfg.debug) >> refreshCache(
              asyncLootAssetLocationCacheRef,
              cfg.debug
            )
          }
        } yield ()
        (Async[F].sleep(cfg.db.attemptSaveIntervalSeconds) >> program).foreverM.start
      }

      taskFiber2 <- LootAssetHighlight.task[F, G](cfg.asset.highlightRefreshInterval).start

      _ <- finalizerRef.set(Some(for {
        _ <- taskFiber1.cancel
        _ <- taskFiber2.cancel
        _ <- saveAssetFromCache(asyncLootAssetLocationCacheRef, cfg.debug)

        _ <- Async[F].delay(CommandAPI.onDisable())
        _ <- logger.info("LootContainerManager disabled.")
      } yield ()))

      _ <- logger.info("LootContainerManager enabled.")
    } yield ()

    val loggerAppliedProgram = for {
      logger <- Slf4jLogger.create[F]
      _      <- program.handleErrorWith { err =>
        logger.error(err)(err.getMessage)
          >> Async[F].delay(Bukkit.getPluginManager.disablePlugin(this))
      }
    } yield ()

    loggerAppliedProgram.unsafeRunSync()
  }

  override def onDisable() = {
    finalizerRef.get.flatMap(_.orEmpty).unsafeRunHereSync()
  }

  def saveAssetFromCache(lootAssetLocationCacheRef: Ref[F, LootAssetCache], debug: Boolean)(using
    logger: Logger[F],
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    cache <- lootAssetLocationCacheRef.get
    _     <- Async[F].whenA(debug)(logger.info("Updating assets..."))
    updatingAssets = for {
      uuid <- cache.updatedAssetLocations.toList
      b    <- cache.mapping.get(uuid).toList
    } yield b
    deletedAssets  = NonEmptyList.fromList(cache.deletedAssetIds.toList)

    _ <- lootAssetRepos.upsertLootAssets(updatingAssets)
    _ <- deletedAssets.traverse_(lootAssetRepos.deleteLootAssets)
  } yield ()

  def refreshCache[F[_]: Async](lootAssetLocationCacheRef: Ref[F, LootAssetCache], debug: Boolean)(using
    logger: Logger[F],
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    _      <- Async[F].whenA(debug)(logger.info("Retrieving assets..."))
    assets <- lootAssetRepos.getAllLootAssets()
    assetsMap    = assets
      .flatMap(a => a.containers.map(_.location -> a.uuid))
      .groupBy(_._1.toChunkLocation)
      .mapV(_.toMap)
    assetMapping = assets.map(a => a.uuid -> a).toMap
    cache        = LootAssetCache(assetsMap, assetMapping, Set.empty, Set.empty)
    _ <- lootAssetLocationCacheRef.set(cache)

    _ <- logger.info(s"Assets retrieved. retrieved: ${assets.size}")
  } yield ()
}
