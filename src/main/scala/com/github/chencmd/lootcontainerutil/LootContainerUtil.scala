package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.adapter.TSBAdapter
import com.github.chencmd.lootcontainerutil.adapter.database.LootAssetRepository
import com.github.chencmd.lootcontainerutil.adapter.database.SQLite
import com.github.chencmd.lootcontainerutil.exceptions.ConfigurationException
import com.github.chencmd.lootcontainerutil.exceptions.UserException
import com.github.chencmd.lootcontainerutil.feature.asset.ItemConversionInstr
import com.github.chencmd.lootcontainerutil.feature.asset.persistence.LootAssetPersistenceInstr
import com.github.chencmd.lootcontainerutil.feature.containerprotection.ProtectActionListener
import com.github.chencmd.lootcontainerutil.generic.EitherTIOExtra.*
import com.github.chencmd.lootcontainerutil.minecraft.ManageItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.ManageBukkitItemNBT
import com.github.chencmd.lootcontainerutil.minecraft.bukkit.OnBukkitServerThread

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.~>

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

  val cmdExecutor: Ref[F, Option[CommandExecutor[F]]] = Ref.unsafe(None)

  override def onEnable() = {
    given OnMinecraftThread[F] = OnBukkitServerThread.createInstr[F](this)
    given ManageItemNBT        = ManageBukkitItemNBT.createInstr

    val program = for {
      cfg <- Config.tryRead[F](this)
      transactor     = SQLite.createTransactor[F](cfg.db)
      lootAssetRepos = LootAssetRepository.createInstr[F](transactor)
      _ <- lootAssetRepos.initialize()
      given LootAssetPersistenceInstr[F] = lootAssetRepos
      given ItemConversionInstr[F]       = TSBAdapter.createInstr[F](this, cfg)
      _ <- cmdExecutor.set(Some(new CommandExecutor))
      pal                  <- ProtectActionListener[F, G](unsafeRunSyncContinuation)
      _                    <- Async[F].delay(Bukkit.getPluginManager.registerEvents(pal, this))
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

  override def onCommand(
    sender: CommandSender,
    command: Command,
    label: String,
    args: Array[String]
  ): Boolean = {
    if (command.getName == "lcu") {
      try {
        cmdExecutor.get.flatMap(_.traverse_(_.run(sender, args.toList))).unsafeRunAndForget()
      } catch {
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
}
