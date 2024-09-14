package com.github.chencmd.lootcontainermanager.generic

type SyncContinuation[F[_], G[_], A] = G[(A, F[Unit])]
