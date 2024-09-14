package com.github.chencmd.lootcontainermanager.adapter.database

import scala.concurrent.duration.FiniteDuration

import java.io.File

case class DBConfig(filePath: File, attemptSaveIntervalSeconds: FiniteDuration)
