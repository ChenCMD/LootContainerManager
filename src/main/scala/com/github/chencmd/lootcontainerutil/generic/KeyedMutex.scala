package com.github.chencmd.lootcontainerutil.generic

import cats.effect.kernel.GenConcurrent
import cats.effect.kernel.Poll
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.implicits.*

trait KeyedMutex[F[_], K, V] {
  def withCancelableLockAtKey[B](key: K)(update: Poll[F] => Option[V] => F[(Option[V], B)]): F[B]

  final def withLockAtKey[B](key: K)(update: Option[V] => F[(Option[V], B)]): F[B] =
    withCancelableLockAtKey(key)(_ => ov => update(ov))
}

object KeyedMutex {
  def empty[F[_], K, V](using gcf: GenConcurrent[F, Throwable]): F[KeyedMutex[F, K, V]] = {
    for {
      mapRef    <- Ref[F].of(Map.empty[K, V])
      semaphore <- Semaphore[F](1)
    } yield new KeyedMutex[F, K, V] {
      def withCancelableLockAtKey[B](key: K)(update: Poll[F] => Option[V] => F[(Option[V], B)]): F[B] =
        semaphore.permit.use { _ =>
          gcf.uncancelable { pollUpdate =>
            for {
              currentMap          <- mapRef.get
              (nextValue, result) <- update(pollUpdate)(currentMap.get(key))
              _                   <- mapRef.set(nextValue match {
                case Some(nv) => currentMap + (key -> nv)
                case None     => currentMap - key
              })
            } yield result
          }
        }
    }
  }
}
