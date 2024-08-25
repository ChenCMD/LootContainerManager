package com.github.chencmd.lootcontainerutil.generic

import cats.effect.kernel.*
import cats.effect.std.Semaphore
import cats.implicits.*

trait Mutex[F[_], V] {
  def accessWithLock[B](
      update: V => F[(V, B)]
  )(using F: MonadCancel[F, ?]): F[B]

  final def updateWithLock(update: V => F[V])(using
      F: MonadCancel[F, ?]
  ): F[Unit] = accessWithLock(v => update(v).tupleRight(()))

  final def readWithLock(using F: MonadCancel[F, ?]): F[V] =
    accessWithLock(v => v.pure[F].tupleRight(v))
}

object Mutex {
  def apply[F[_], V](
      v: V
  )(using F: GenConcurrent[F, Throwable]): F[Mutex[F, V]] = for {
    semaphore <- Semaphore[F](1)
    ref <- Ref[F].of(v)
  } yield new Mutex[F, V] {
    def accessWithLock[B](update: V => F[(V, B)])(using
        F: MonadCancel[F, ?]
    ): F[B] = semaphore.permit.use { _ =>
      F.uncancelable { poll =>
        for {
          v <- ref.get
          // NOTE: When the inner computation gets canceled, this mutex gets poisoned
          //       but we'll ignore that case for now
          (newV, b) <- poll(update(v))
          _ <- ref.set(newV)
        } yield b
      }
    }
  }
}
