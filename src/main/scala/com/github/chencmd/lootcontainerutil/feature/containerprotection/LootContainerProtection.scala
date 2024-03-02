package com.github.chencmd.lootcontainerutil.feature.containerprotection

import com.github.chencmd.lootcontainerutil.Prefix
import com.github.chencmd.lootcontainerutil.generic.OptionTExtra
import com.github.chencmd.lootcontainerutil.generic.SyncContinuation
import com.github.chencmd.lootcontainerutil.generic.extensions.CastOps.downcastOrNone
import com.github.chencmd.lootcontainerutil.minecraft.OnMinecraftThread

import cats.data.OptionT
import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import scala.concurrent.duration.*
import scala.util.chaining.*

import org.bukkit.Sound
import org.bukkit.SoundCategory
import org.bukkit.block.Container
import org.bukkit.event.block.Action
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.loot.LootTable
import org.bukkit.loot.Lootable

object LootContainerProtection {
  def onPlayerInteract[F[_]: Async](
    e: PlayerInteractEvent
  )(using mcThread: OnMinecraftThread[F]): SyncContinuation[F, Boolean] = {
    val p = e.getPlayer

    val lootTable = {
      val program = for {
        _ <- OptionTExtra.exitWhenF(e.getAction != Action.RIGHT_CLICK_BLOCK)(SyncIO.unit)
        hasItemInHand = p.getInventory().pipe { inv =>
          !inv.getItemInMainHand.getType.isAir || !inv.getItemInOffHand.getType.isAir
        }
        _         <- OptionTExtra.exitWhenF(p.isSneaking() && hasItemInHand)(SyncIO.unit)
        container <- OptionT
          .fromOption[SyncIO](Option(e.getClickedBlock.getState))
          .flatMap(block => OptionT.fromOption(block.downcastOrNone[Container]))
          .flatMap(container => OptionT.fromOption(container.downcastOrNone[Lootable]))
        lootTable <- OptionT.fromOption[SyncIO](Option(container.getLootTable))
      } yield lootTable
      program.value
    }

    def effect(lootTable: LootTable): F[Unit] = for {
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.INFO}ルートテーブルが設定されているため開くことができませんでした。"))
      _ <- Async[F].delay(p.sendMessage(s"${Prefix.INFO}設定されているルートテーブル: ${lootTable.getKey}"))
      loc = p.getLocation
      _ <- Async[F].delay(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
      _ <- Async[F].sleep(0.08.seconds)
      _ <- Async[F].delay(p.playSound(loc, Sound.BLOCK_NOTE_BLOCK_PLING, SoundCategory.MASTER, 0.6f, 2))
    } yield ()

    lootTable.map(lt => (lt.nonEmpty, lt.traverse_(effect)))
  }
}
