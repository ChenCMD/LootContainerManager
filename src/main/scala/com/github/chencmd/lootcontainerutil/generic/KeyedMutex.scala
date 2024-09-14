package com.github.chencmd.lootcontainermanager.generic

import com.github.chencmd.lootcontainermanager.generic.extensions.OptionExt.*

import cats.effect.kernel.GenConcurrent
import cats.effect.kernel.Ref
import cats.implicits.*

trait KeyedMutex[F[_], K, V] {
  def withLockAtKey[B](key: K)(update: Option[V] => F[(Option[V], B)]): F[B]
}

object KeyedMutex {
  private case class MapEntry[F[_], V](
    // must be protected by the outer mutex
    waiterCounter: Ref[F, Int],
    recordMutex: Mutex[F, Option[V]]
  )

  private object MapEntry {
    def allocateEmpty[F[_], V](using F: GenConcurrent[F, Throwable]): F[MapEntry[F, V]] = {
      (Ref[F].of(0), Mutex[F, Option[V]](Option.empty)).mapN(MapEntry.apply[F, V])
    }
  }

  def empty[F[_], K, V](using F: GenConcurrent[F, Throwable]): F[KeyedMutex[F, K, V]] = {
    Mutex[F, Map[K, MapEntry[F, V]]](Map.empty).map { mapMutex =>
      new KeyedMutex[F, K, V] {
        def withLockAtKey[B](key: K)(update: Option[V] => F[(Option[V], B)]): F[B] = F.uncancelable { poll =>
          for {
            // prepare locked access to the entry
            MapEntry(waiterCounter, recordMutex) <- poll {
              mapMutex.accessWithLock { mapAtFirst =>
                F.uncancelable { _ =>
                  for {
                    entryAtKey <- mapAtFirst.get(key).getOrElseEffectfully(MapEntry.allocateEmpty[F, V])
                    _          <- entryAtKey.waiterCounter.update(_ + 1)
                  } yield (mapAtFirst + (key -> entryAtKey), entryAtKey)
                }
              }
            }
            b                                    <- F.guarantee(
              poll(recordMutex.accessWithLock(update)),
              // cleanup the empty entry if we are the last waiter
              mapMutex.updateWithLock { map =>
                for {
                  waiters                <- waiterCounter.updateAndGet(_ - 1)
                  // if there are no concurrent access (that have incremented waiterCounter but not yet decremented it),
                  // i.e. waiters == 0, and the entry is empty (if waiters == 0, recordMutex.readWithLock should return without blocking,
                  // with a result of None if our access wanted to unset the entry), we must remove the entry.
                  // Otherwise, we should let another future access try removing the entry.
                  weMustRemoveKeyFromMap <- {
                    if (waiters == 0) {
                      recordMutex.readWithLock.map(_.isEmpty)
                    } else {
                      false.pure[F]
                    }
                  }
                } yield if (weMustRemoveKeyFromMap) map - key else map
              }
            )
          } yield b
        }
      }
    }
  }
}
