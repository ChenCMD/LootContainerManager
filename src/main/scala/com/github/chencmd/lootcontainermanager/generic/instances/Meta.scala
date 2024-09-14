package com.github.chencmd.lootcontainermanager.generic.instances

import java.util.UUID

import doobie.Meta
import doobie.enumerated.JdbcType

object MetaInstances {
  given Meta[UUID] = Meta.Basic.one[UUID](
    JdbcType.Char,
    List.empty,
    (rs, i) => UUID.fromString(rs.getString(i)),
    (ps, i, a) => ps.setString(i, a.toString),
    (rs, i, a) => rs.updateString(i, a.toString)
  )
}
