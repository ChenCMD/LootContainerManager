package com.github.chencmd.lootcontainerutil.generic.givens

import cats.Eq
import CanEquals.given
import org.bukkit.entity.Player

import java.util.UUID

object PlayerCompare {
  given Eq[Player] = {
    Eq.instance(_.getUniqueId == _.getUniqueId)
  }
}
