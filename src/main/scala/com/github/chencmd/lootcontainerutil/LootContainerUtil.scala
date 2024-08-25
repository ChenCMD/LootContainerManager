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
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.BlockLocation
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.InventorySession
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.ManageBukkitItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.OnBukkitServerThread
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

import java.util.logging.Level
import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin
import dev.jorel.commandapi.CommandAPI
import dev.jorel.commandapi.CommandAPIBukkitConfig

class LootContainerUtil extends JavaPlugin {
  type F = IO[_]
  type G = SyncIO[_]
  val coerceF: G ~> F           = FunctionK.lift([A] => (_: G[A]).to[F])
  val unsafeRunAsync            =
    [U] => (errorHandler: Throwable => U) => [U1] => (fa: F[U1]) => fa.unsafeRunAsync(_.left.foreach(errorHandler))
  val unsafeRunSyncContinuation = [A] =>
    (cont: SyncContinuation[F, G, A]) => {
      val (a, effect) = cont.unsafeRunSync()
      effect.unsafeRunAndForget()
      a
  }

  val finalizerRef: Ref[F, Option[F[Unit]]]              = Ref.unsafe(None)

  override def onLoad() = {
    CommandAPI.onLoad(CommandAPIBukkitConfig(this))
  }

  override def onEnable() = {
    given OnMinecraftThread[F] = OnBukkitServerThread.createInstr[F](this)
    given ManageItemNBT        = ManageBukkitItemNBT.createInstr

    val program = for {
      cfg <- Config.tryRead[F](this)
      transactor     = SQLite.createTransactor[F](cfg.db)
      lootAssetRepos = LootAssetRepository.createInstr[F](transactor)

      given LootAssetPersistenceInstr[F] = lootAssetRepos
      given ItemConversionInstr[F]       = TSBAdapter.createInstr[F](this, cfg)

      syncLootAssetLocationCacheRef <- Ref.in[F, G, LootAssetCache](LootAssetCache.empty)
      asyncLootAssetLocationCacheRef          = syncLootAssetLocationCacheRef.mapK(coerceF)
      given LootAssetPersistenceCacheInstr[G] = LootAssetRepositoryCache.createInstr[G](syncLootAssetLocationCacheRef)
      given LootAssetPersistenceCacheInstr[F] = LootAssetRepositoryCache.createInstr[F](asyncLootAssetLocationCacheRef)

      openedInventoriesRef <- Ref.of[F, Map[BlockLocation, InventorySession]](Map.empty)
      pal                  <- ProtectActionListener[F, G](unsafeRunSyncContinuation)
      cml                  <- ContainerManageListener[F, G](openedInventoriesRef)(unsafeRunSyncContinuation)
      _                    <- Async[F].delay(Bukkit.getPluginManager.registerEvents(pal, this))
      _                    <- Async[F].delay(Bukkit.getPluginManager.registerEvents(cml, this))

      _ <- Async[F].delay(CommandAPI.onEnable())
      _ <- CommandExecutor.register[F](openedInventoriesRef, unsafeRunAsync)

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
      } yield ()))

      _ <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("LootContainerUtil enabled."))
    } yield ()

    try {
      program.unsafeRunSync()
    } catch {
      case err =>
        Bukkit.getLogger().log(Level.SEVERE, err.getMessage, err)
        Bukkit.getPluginManager.disablePlugin(this)
    }
  }

  override def onDisable() = {
    finalizerRef.get.flatMap(_.orEmpty).unsafeRunSync()
    CommandAPI.onDisable()
    Bukkit.getConsoleSender.sendMessage("LootContainerUtil disabled.")
  }

  def saveAssetFromCache(lootAssetLocationCacheRef: Ref[F, LootAssetCache])(using
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    cache <- lootAssetLocationCacheRef.get
    _     <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Updating assets..."))
    updatingAssets = for {
      uuid <- cache.updatedAssetLocations.toList
      b    <- cache.mapping.get(uuid).toList
    } yield b
    deletedAssets  = NonEmptyList.fromList(cache.deletedAssetIds.toList)

    _ <- lootAssetRepos.upsertLootAssets(updatingAssets)
    _ <- deletedAssets.traverse_(lootAssetRepos.deleteLootAssets)
  } yield ()

  def refreshCache[F[_]: Async](lootAssetLocationCacheRef: Ref[F, LootAssetCache])(using
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    _      <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Retrieving assets..."))
    assets <- lootAssetRepos.getAllLootAssets()
    assetsMap    = assets
      .flatMap(a => a.containers.map(_.location -> a.uuid))
      .groupBy(_._1.toChunkLocation)
      .mapV(_.toMap)
    assetMapping = assets.map(a => a.uuid -> a).toMap
    cache        = LootAssetCache(assetsMap, assetMapping, Set.empty, Set.empty)
    _ <- lootAssetLocationCacheRef.set(cache)

    _ <- Async[F].delay {
      Bukkit.getConsoleSender.sendMessage("Assets retrieved.")
      Bukkit.getConsoleSender.sendMessage("Assets:")
      assets.foreach(asset => Bukkit.getConsoleSender.sendMessage(asset.toString))
    }
  } yield ()
}
