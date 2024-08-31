package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread

import cats.effect.kernel.Async
import cats.implicits.*
import cats.~>
import org.typelevel.log4cats.Logger

import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin
import cats.effect.kernel.Sync

object OnBukkitServerThread {
  def createInstr[F[_]: Async, G[_]: Sync](taskOwner: JavaPlugin)(unsafeRunSync: [A] => G[A] => A, coerceF: G ~> F)(
    using logger: Logger[F]
  ): OnMinecraftThread[F, G] = new OnMinecraftThread[F, G] {
    private val errorHandler: PartialFunction[Throwable, F[Unit]] = {
      case e: Throwable => logger.error(e)(e.getMessage())
    }

    def run[A](syncAction: G[A]): F[A] = {
      val tryRunning: G[Option[A]] = Sync[G].delay {
        Option.when(Bukkit.getServer.isPrimaryThread)(unsafeRunSync(syncAction))
      }

      coerceF(tryRunning).flatMap {
        case Some(value) => value.pure[F]
        case None        => Async[F].async[A] { callback =>
            Async[F].delay {
              val runnable = makeRunnable(syncAction, callback)
              val task     = Bukkit.getScheduler.runTask(taskOwner, runnable)
              Async[F].delay(task.cancel()).some
            }
          }
      }
    }

    def runAndForget[A](syncAction: G[A]): F[Unit] = {
      Async[F].start(run(syncAction)).onError(errorHandler).void
    }

    def runLater[A](delay: Long)(syncAction: G[A]): F[A] = {
      Async[F].async { callback =>
        Async[F].delay {
          val runnable = makeRunnable(syncAction, callback)
          val task     = Bukkit.getScheduler.runTaskLater(taskOwner, runnable, delay)
          Async[F].delay(task.cancel()).some
        }
      }
    }

    def runLaterAndForget[A](delay: Long)(syncAction: G[A]): F[Unit] = {
      Async[F].start(runLater(delay)(syncAction)).onError(errorHandler).void
    }

    private def makeRunnable[A](syncAction: G[A], callback: Either[Throwable, A] => Unit): Runnable = { () =>
      callback {
        try Right(unsafeRunSync(syncAction))
        catch e => Left(e)
      }
    }
  }
}
