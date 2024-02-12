package com.github.chencmd.lootcontainerutil.generic

import cats.effect.SyncIO

type SyncContinuation[F[_], A] = SyncIO[(A, F[Unit])]
