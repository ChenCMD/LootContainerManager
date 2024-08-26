package com.github.chencmd.lootcontainermanager.minecraft.bukkit

import com.github.chencmd.lootcontainermanager.minecraft.OnMinecraftThread

import cats.effect.SyncIO
import cats.effect.kernel.Async
import cats.implicits.*

import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin
import org.typelevel.log4cats.Logger

object OnBukkitServerThread {
  def createInstr[F[_]: Async](taskOwner: JavaPlugin)(using
    logger: Logger[F]
  ): OnMinecraftThread[F] = new OnMinecraftThread[F] {
    private val errorHandler: PartialFunction[Throwable, F[Unit]] = {
      case e: Throwable => logger.error(e)(e.getMessage())
    }

    def run[A](syncAction: SyncIO[A]): F[A] = {
      val tryRunning: SyncIO[Option[A]] = SyncIO {
        Option.when(Bukkit.getServer.isPrimaryThread)(syncAction.unsafeRunSync())
      }

      tryRunning.to[F].flatMap {
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

    def runAndForget[A](syncAction: SyncIO[A]): F[Unit] = {
      Async[F].start(run(syncAction)).onError(errorHandler).void
    }

    def runLater[A](delay: Long)(syncAction: SyncIO[A]): F[A] = {
      Async[F].async { callback =>
        Async[F].delay {
          val runnable = makeRunnable(syncAction, callback)
          val task     = Bukkit.getScheduler.runTaskLater(taskOwner, runnable, delay)
          Async[F].delay(task.cancel()).some
        }
      }
    }

    def runLaterAndForget[A](delay: Long)(syncAction: SyncIO[A]): F[Unit] = {
      Async[F].start(runLater(delay)(syncAction)).onError(errorHandler).void
    }

    private def makeRunnable[A](
      syncAction: SyncIO[A],
      callback: Either[Throwable, A] => Unit
    ): Runnable = { () =>
      callback {
        try Right(syncAction.unsafeRunSync())
        catch e => Left(e)
      }
    }
  }
}
