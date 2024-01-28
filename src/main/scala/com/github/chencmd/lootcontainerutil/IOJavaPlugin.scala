package com.github.chencmd.lootcontainerutil

import cats.implicits.*

import org.bukkit.plugin.java.JavaPlugin
import cats.effect.IO
import org.bukkit.command.Command
import org.bukkit.command.CommandSender
import cats.effect.unsafe.implicits.global
import cats.kernel.Monoid
import org.bukkit.command.PluginCommand
import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.generator.BiomeProvider
import org.bukkit.generator.ChunkGenerator
import java.util.logging.Logger
import scala.jdk.CollectionConverters.*
import java.io.InputStream

enum Effect[A] {
  case Pure(value: A)
  case Sync(effect: IO[A])
  case Async(effect: IO[Unit])

  def unsafeRun()(using M: Monoid[A]): A = unsafeRun(M.empty)

  def unsafeRun(resultIfAsync: => A): A = this match {
    case Pure(value)   => value
    case Sync(effect)  => effect.unsafeRunSync()
    case Async(effect) =>
      effect.unsafeRunAndForget()
      resultIfAsync
  }
}

trait IOJavaPlugin extends JavaPlugin {
  type F = IO[_]

  def onEnableIO(): Effect[Unit] = Effect.Pure(super.onEnable())

  def onCommandIO(
    sender: CommandSender,
    command: Command,
    label: String,
    args: Array[String]
  ): Effect[Boolean] = Effect.Pure(super.onCommand(sender, command, label, args))

  def getCommandIO(name: String): IO[PluginCommand] = IO.pure(super.getCommand(name))

  def getConfigIO(): IO[FileConfiguration] = IO.pure(super.getConfig())

  def getDefaultBiomeProviderIO(worldName: String, id: String): IO[BiomeProvider] =
    IO.pure(super.getDefaultBiomeProvider(worldName, id))

  def getDefaultWorldGeneratorIO(worldName: String, id: String): IO[ChunkGenerator] =
    IO.pure(super.getDefaultWorldGenerator(worldName, id))

  def getLoggerIO(): IO[Logger] = IO.pure(super.getLogger())

  def getResourceIO(fileName: String): IO[InputStream] = IO.pure(super.getResource(fileName))

  def onDisableIO(): Effect[Unit] = Effect.Pure(super.onDisable())

  def onLoadIO(): Effect[Unit] = Effect.Pure(super.onLoad())

  def onTabCompleteIO(
    sender: CommandSender,
    command: Command,
    alias: String,
    args: Array[String]
  ): Effect[List[String]] = Effect.Pure(super.onTabComplete(sender, command, alias, args).asScala.toList)

  def reloadConfigIO(): Effect[Unit] = Effect.Pure(super.reloadConfig())

  def saveConfigIO(): Effect[Unit] = Effect.Pure(super.saveConfig())

  def saveDefaultConfigIO(): Effect[Unit] = Effect.Pure(super.saveDefaultConfig())

  def saveResourceIO(resourcePath: String, replace: Boolean): Effect[Unit] =
    Effect.Pure(super.saveResource(resourcePath, replace))

  final override def onEnable(): Unit = onEnableIO().unsafeRun()

  final override def onCommand(sender: CommandSender, command: Command, label: String, args: Array[String]): Boolean =
    onCommandIO(sender, command, label, args).unsafeRun(true)

  final override def getCommand(name: String): PluginCommand = getCommandIO(name).unsafeRunSync()

  final override def getConfig(): FileConfiguration = getConfigIO().unsafeRunSync()

  final override def getDefaultBiomeProvider(worldName: String, id: String): BiomeProvider =
    getDefaultBiomeProviderIO(worldName, id).unsafeRunSync()

  final override def getDefaultWorldGenerator(worldName: String, id: String): ChunkGenerator =
    getDefaultWorldGeneratorIO(worldName, id).unsafeRunSync()

  final override def getLogger(): Logger = getLoggerIO().unsafeRunSync()

  final override def getResource(fileName: String): InputStream = getResourceIO(fileName).unsafeRunSync()

  final override def onDisable(): Unit = onDisableIO().unsafeRun()

  final override def onLoad(): Unit = onLoadIO().unsafeRun()

  final override def onTabComplete(
    sender: CommandSender,
    command: Command,
    label: String,
    args: Array[String]
  ): java.util.List[String] = onTabCompleteIO(sender, command, label, args).unsafeRun().asJava

  final override def reloadConfig(): Unit = reloadConfigIO().unsafeRun()

  final override def saveConfig(): Unit = saveConfigIO().unsafeRun()

  final override def saveDefaultConfig(): Unit = saveDefaultConfigIO().unsafeRun()

  final override def saveResource(resourcePath: String, replace: Boolean): Unit =
    saveResourceIO(resourcePath, replace).unsafeRun()

}
