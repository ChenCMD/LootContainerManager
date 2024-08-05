package com.github.chencmd.lootcontainerutil.generic

type SyncContinuation[F[_], G[_], A] = G[(A, F[Unit])]
