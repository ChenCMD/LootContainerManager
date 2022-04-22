package com.github.chencmd.lootcontainerutil.minecraft

import cats.effect.kernel.Async
import cats.effect.{IO, SyncIO}
import cats.implicits.given

import org.bukkit.Bukkit
import org.bukkit.plugin.java.JavaPlugin

class OnMinecraftThread[F[_]: Async](taskOwner: JavaPlugin) {
  private val errorHandler: PartialFunction[Throwable, F[Unit]] = { case e: Throwable =>
    SyncIO(Bukkit.getLogger.finest(e.getStackTrace.mkString("\n"))).to[F]
  }

  def run[A](syncAction: SyncIO[A]): F[A] = {
    val tryRunning: SyncIO[Option[A]] = SyncIO {
      if Bukkit.getServer.isPrimaryThread then Some(syncAction.unsafeRunSync())
      else None
    }

    Async[F].flatMap(tryRunning.to[F]) { opt =>
      val whenEmpty = Async[F].async[A] { callback =>
        Async[F].delay {
          val runnable = makeRunnable(syncAction, callback)

          val task = Bukkit.getScheduler.runTask(taskOwner, runnable)

          Some(Async[F].delay {
            task.cancel()
          })
        }
      }
      opt.fold[F[A]](whenEmpty)(Async[F].pure)
    }
  }

  def runAndForget[A](syncAction: SyncIO[A]): F[Unit] = {
    Async[F].start(run(syncAction)).onError(errorHandler).void
  }

  def runLater[A](delay: Long)(syncAction: SyncIO[A]): F[A] = {
    Async[F].async { callback =>
      Async[F].delay {
        val runnable = makeRunnable(syncAction, callback)

        val task = Bukkit.getScheduler.runTaskLater(taskOwner, runnable, delay)

        Some(Async[F].delay {
          task.cancel()
        })
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
