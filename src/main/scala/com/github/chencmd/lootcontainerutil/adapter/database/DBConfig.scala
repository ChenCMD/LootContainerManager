package com.github.chencmd.lootcontainermanager.adapter.database

import java.io.File
import scala.concurrent.duration.FiniteDuration

case class DBConfig(filePath: File, attemptSaveIntervalSeconds: FiniteDuration)
