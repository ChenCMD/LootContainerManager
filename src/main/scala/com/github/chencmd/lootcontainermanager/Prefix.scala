package com.github.chencmd.lootcontainermanager

import org.bukkit.ChatColor

object Prefix {
  final val ERROR   = s"${ChatColor.RED}Error >> ${ChatColor.RESET}"
  final val WARNING = s"${ChatColor.YELLOW}Warning >> ${ChatColor.RESET}"
  final val SUCCESS = s"${ChatColor.GREEN}Success >> ${ChatColor.RESET}"
  final val INFO    = s"${ChatColor.AQUA}Information >> ${ChatColor.RESET}"
}
