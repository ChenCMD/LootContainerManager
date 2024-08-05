package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.adapter.TSBAdapter
import com.github.chencmd.lootcontainerutil.adapter.database.LootAssetRepository
import com.github.chencmd.lootcontainerutil.adapter.database.LootAssetRepositoryCache
import com.github.chencmd.lootcontainerutil.adapter.database.SQLite
import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceCacheInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.feature.containerprotection.ProtectActionListener
import com.github.chencmd.lootcontainerutil.generic.EitherTIOExtra.*
import com.github.chencmd.lootcontainerutil.generic.MapExtra.*
import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
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

import doobie.*
import org.bukkit.Bukkit
import org.bukkit.command.Command
import org.bukkit.command.CommandSender
import org.bukkit.plugin.java.JavaPlugin

class LootContainerUtil extends JavaPlugin {
  type F = IO[_]
  type G = SyncIO[_]
  val coerceF: G ~> F           = FunctionK.lift([A] => (_: G[A]).to[F])
  val unsafeRunSyncContinuation = [A] =>
    (cont: SyncContinuation[F, G, A]) => {
      val (a, effect) = cont.unsafeRunSync()
      effect.unsafeRunAndForget()
      a
  }

  val cmdExecutorRef: Ref[F, Option[CommandExecutor[F]]] = Ref.unsafe(None)
  val finalizerRef: Ref[F, Option[F[Unit]]]              = Ref.unsafe(None)

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

      pal                  <- ProtectActionListener[F, G](unsafeRunSyncContinuation)
      _                    <- Async[F].delay(Bukkit.getPluginManager.registerEvents(pal, this))

      cmdExecutor <- CommandExecutor[F]
      _           <- cmdExecutorRef.set(Some(cmdExecutor))

      _          <- lootAssetRepos.initialize()
      _          <- refreshCache(asyncLootAssetLocationCacheRef)
      taskFiber1 <- {
        val program = for {
          _     <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Starting cache update task"))
          cache <- asyncLootAssetLocationCacheRef.get
          _     <- Async[F].whenA(cache.updatedAssetLocations.nonEmpty || cache.deletedAssetIds.nonEmpty) {
            saveAssetFromCache(asyncLootAssetLocationCacheRef) >> refreshCache(asyncLootAssetLocationCacheRef)
          }
          _     <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Cache update task completed"))
        } yield ()
        (Async[F].sleep(30.seconds) >> program).foreverM.start
      }

      _ <- finalizerRef.set(Some(for {
        _ <- taskFiber1.cancel
        _ <- saveAssetFromCache(asyncLootAssetLocationCacheRef)
      } yield ()))

      _ <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("LootContainerUtil enabled."))
    } yield ()

    try {
      program.unsafeRunSync()
    } catch {
      case err: ConfigurationException => Bukkit.getConsoleSender.sendMessage(err.getMessage)
      case err                         =>
        Bukkit.getConsoleSender.sendMessage(err.getMessage)
        Bukkit.getPluginManager.disablePlugin(this)
    }
  }

  override def onDisable() = {
    finalizerRef.get.flatMap(_.orEmpty).unsafeRunSync()
    Bukkit.getConsoleSender.sendMessage("LootContainerUtil disabled.")
  }

  override def onCommand(
    sender: CommandSender,
    command: Command,
    label: String,
    args: Array[String]
  ): Boolean = {
    if (command.getName == "lcu") {
      val program = cmdExecutorRef.get.flatMap(_.traverse_(_.run(sender, args.toList)))
      program.unsafeRunAsync(_.left.foreach {
        case err: UserException          => sender.sendMessage(err.getMessage)
        case err: ConfigurationException =>
          sender.sendMessage("An error occurred while loading the configuration file.")
          Bukkit.getConsoleSender.sendMessage(err.getMessage)
        case err                         =>
          sender.sendMessage("An error occurred while executing the command.")
          Bukkit.getConsoleSender.sendMessage(err.getMessage)
      }
      true
    } else {
      false
    }
  }

  def saveAssetFromCache(lootAssetLocationCacheRef: Ref[F, LootAssetCache])(using
    lootAssetRepos: LootAssetPersistenceInstr[F]
  ): F[Unit] = for {
    cache <- lootAssetLocationCacheRef.get
    _     <- Async[F].delay(Bukkit.getConsoleSender.sendMessage("Updating assets..."))
    updatingAssets = for {
      loc <- cache.updatedAssetLocations.toList
      b   <- cache.assets(loc.toChunkLocation).get(loc)
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
    assetsMap = assets.groupBy(_.location.toChunkLocation).mapV(v => Map(v.map(a => a.location -> a)*))
    _ <- lootAssetLocationCacheRef.set(LootAssetCache(assetsMap, Set.empty, Set.empty))

    _ <- Async[F].delay {
      Bukkit.getConsoleSender.sendMessage("Assets retrieved.")
      Bukkit.getConsoleSender.sendMessage("Assets:")
      assets.foreach(asset => Bukkit.getConsoleSender.sendMessage(asset.toString))
    }
  } yield ()
}
